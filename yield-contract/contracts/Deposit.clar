;; Yield Pool Smart Contract
;; Secure deposit and withdrawal functions for yield farming

;; Define SIP-010 trait inline instead of importing from external contract
(define-trait sip-010-trait
  (
    (transfer (uint principal principal (optional (buff 34))) (response bool uint))
    (get-name () (response (string-ascii 32) uint))
    (get-symbol () (response (string-ascii 32) uint))
    (get-decimals () (response uint uint))
    (get-balance (principal) (response uint uint))
    (get-total-supply () (response uint uint))
    (get-token-uri () (response (optional (string-utf8 256)) uint))
  )
)

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-OWNER-ONLY (err u100))
(define-constant ERR-POOL-NOT-FOUND (err u101))
(define-constant ERR-INSUFFICIENT-BALANCE (err u102))
(define-constant ERR-MINIMUM-DEPOSIT (err u103))
(define-constant ERR-POOL-PAUSED (err u104))
(define-constant ERR-LOCK-PERIOD-ACTIVE (err u105))
(define-constant ERR-INVALID-AMOUNT (err u106))
(define-constant ERR-POOL-EXISTS (err u107))
(define-constant ERR-INSUFFICIENT-SHARES (err u108))

;; Data Variables
(define-data-var protocol-fee-rate uint u250) ;; 2.5% in basis points
(define-data-var next-pool-id uint u1)

;; Data Maps
(define-map pools
  { pool-id: uint }
  {
    token-contract: principal,
    total-deposits: uint,
    total-shares: uint,
    yield-rate: uint, ;; Annual yield rate in basis points
    lock-period: uint, ;; Lock period in blocks
    is-active: bool,
    created-at: uint
  }
)

(define-map user-deposits
  { user: principal, pool-id: uint }
  {
    shares: uint,
    deposit-amount: uint,
    deposit-block: uint,
    last-claim-block: uint
  }
)

(define-map pool-metadata
  { pool-id: uint }
  {
    name: (string-ascii 50),
    description: (string-ascii 200),
    minimum-deposit: uint
  }
)

;; Read-only functions

(define-read-only (get-pool (pool-id uint))
  (map-get? pools { pool-id: pool-id })
)

(define-read-only (get-user-deposit (user principal) (pool-id uint))
  (map-get? user-deposits { user: user, pool-id: pool-id })
)

(define-read-only (get-pool-metadata (pool-id uint))
  (map-get? pool-metadata { pool-id: pool-id })
)

(define-read-only (calculate-yield (user principal) (pool-id uint))
  (let (
    (user-data (unwrap! (get-user-deposit user pool-id) (err u0)))
    (pool-data (unwrap! (get-pool pool-id) (err u0)))
    (blocks-elapsed (- block-height (get last-claim-block user-data)))
    (annual-blocks u52560) ;; Approximate blocks per year (10 min blocks)
    (yield-rate (get yield-rate pool-data))
    (deposit-amount (get deposit-amount user-data))
  )
    (ok (/ (* (* deposit-amount yield-rate) blocks-elapsed) (* annual-blocks u10000)))
  )
)

(define-read-only (get-protocol-fee-rate)
  (var-get protocol-fee-rate)
)

;; Private functions

(define-private (is-owner)
  (is-eq tx-sender CONTRACT-OWNER)
)

(define-private (calculate-shares (amount uint) (pool-id uint))
  (let (
    (pool-data (unwrap! (get-pool pool-id) (err u0)))
    (total-deposits (get total-deposits pool-data))
    (total-shares (get total-shares pool-data))
  )
    (if (is-eq total-shares u0)
      (ok amount) ;; First deposit, 1:1 ratio
      (ok (/ (* amount total-shares) total-deposits))
    )
  )
)

;; Public functions

(define-public (create-pool 
  (token-contract <sip-010-trait>)
  (yield-rate uint)
  (lock-period uint)
  (name (string-ascii 50))
  (description (string-ascii 200))
  (minimum-deposit uint)
)
  (let (
    (pool-id (var-get next-pool-id))
  )
    (asserts! (is-owner) ERR-OWNER-ONLY)
    (asserts! (is-none (get-pool pool-id)) ERR-POOL-EXISTS)
    
    ;; Create pool
    (map-set pools
      { pool-id: pool-id }
      {
        token-contract: (contract-of token-contract),
        total-deposits: u0,
        total-shares: u0,
        yield-rate: yield-rate,
        lock-period: lock-period,
        is-active: true,
        created-at: block-height
      }
    )
    
    ;; Set metadata
    (map-set pool-metadata
      { pool-id: pool-id }
      {
        name: name,
        description: description,
        minimum-deposit: minimum-deposit
      }
    )
    
    ;; Increment pool ID
    (var-set next-pool-id (+ pool-id u1))
    
    (ok pool-id)
  )
)

(define-public (deposit (token-contract <sip-010-trait>) (pool-id uint) (amount uint))
  (let (
    (pool-data (unwrap! (get-pool pool-id) ERR-POOL-NOT-FOUND))
    (pool-meta (unwrap! (get-pool-metadata pool-id) ERR-POOL-NOT-FOUND))
    (shares (unwrap! (calculate-shares amount pool-id) ERR-INVALID-AMOUNT))
    (existing-deposit (default-to 
      { shares: u0, deposit-amount: u0, deposit-block: u0, last-claim-block: u0 }
      (get-user-deposit tx-sender pool-id)
    ))
  )
    ;; Validations
    (asserts! (get is-active pool-data) ERR-POOL-PAUSED)
    (asserts! (>= amount (get minimum-deposit pool-meta)) ERR-MINIMUM-DEPOSIT)
    (asserts! (is-eq (contract-of token-contract) (get token-contract pool-data)) ERR-POOL-NOT-FOUND)
    
    ;; Transfer tokens from user to contract
    (try! (contract-call? token-contract transfer amount tx-sender (as-contract tx-sender) none))
    
    ;; Update pool data
    (map-set pools
      { pool-id: pool-id }
      (merge pool-data {
        total-deposits: (+ (get total-deposits pool-data) amount),
        total-shares: (+ (get total-shares pool-data) shares)
      })
    )
    
    ;; Update user deposit
    (map-set user-deposits
      { user: tx-sender, pool-id: pool-id }
      {
        shares: (+ (get shares existing-deposit) shares),
        deposit-amount: (+ (get deposit-amount existing-deposit) amount),
        deposit-block: block-height,
        last-claim-block: block-height
      }
    )
    
    (ok shares)
  )
)

(define-public (withdraw (token-contract <sip-010-trait>) (pool-id uint) (shares-to-withdraw uint))
  (let (
    (pool-data (unwrap! (get-pool pool-id) ERR-POOL-NOT-FOUND))
    (user-data (unwrap! (get-user-deposit tx-sender pool-id) ERR-INSUFFICIENT-SHARES))
    (user-shares (get shares user-data))
    (total-shares (get total-shares pool-data))
    (total-deposits (get total-deposits pool-data))
    (withdrawal-amount (/ (* shares-to-withdraw total-deposits) total-shares))
    (yield-amount (unwrap! (calculate-yield tx-sender pool-id) ERR-INVALID-AMOUNT))
    (protocol-fee (/ (* yield-amount (var-get protocol-fee-rate)) u10000))
    (net-yield (- yield-amount protocol-fee))
    (total-withdrawal (+ withdrawal-amount net-yield))
  )
    ;; Validations
    (asserts! (get is-active pool-data) ERR-POOL-PAUSED)
    (asserts! (<= shares-to-withdraw user-shares) ERR-INSUFFICIENT-SHARES)
    (asserts! (>= block-height (+ (get deposit-block user-data) (get lock-period pool-data))) ERR-LOCK-PERIOD-ACTIVE)
    (asserts! (is-eq (contract-of token-contract) (get token-contract pool-data)) ERR-POOL-NOT-FOUND)
    
    ;; Transfer tokens back to user
    (try! (as-contract (contract-call? token-contract transfer total-withdrawal tx-sender CONTRACT-OWNER none)))
    
    ;; Update pool data
    (map-set pools
      { pool-id: pool-id }
      (merge pool-data {
        total-deposits: (- (get total-deposits pool-data) withdrawal-amount),
        total-shares: (- (get total-shares pool-data) shares-to-withdraw)
      })
    )
    
    ;; Update user deposit
    (if (is-eq shares-to-withdraw user-shares)
      ;; Complete withdrawal - remove user data
      (map-delete user-deposits { user: tx-sender, pool-id: pool-id })
      ;; Partial withdrawal - update user data
      (map-set user-deposits
        { user: tx-sender, pool-id: pool-id }
        (merge user-data {
          shares: (- user-shares shares-to-withdraw),
          deposit-amount: (- (get deposit-amount user-data) withdrawal-amount),
          last-claim-block: block-height
        })
      )
    )
    
    (ok total-withdrawal)
  )
)

(define-public (claim-yield (token-contract <sip-010-trait>) (pool-id uint))
  (let (
    (pool-data (unwrap! (get-pool pool-id) ERR-POOL-NOT-FOUND))
    (user-data (unwrap! (get-user-deposit tx-sender pool-id) ERR-INSUFFICIENT-SHARES))
    (yield-amount (unwrap! (calculate-yield tx-sender pool-id) ERR-INVALID-AMOUNT))
    (protocol-fee (/ (* yield-amount (var-get protocol-fee-rate)) u10000))
    (net-yield (- yield-amount protocol-fee))
  )
    ;; Validations
    (asserts! (get is-active pool-data) ERR-POOL-PAUSED)
    (asserts! (> yield-amount u0) ERR-INVALID-AMOUNT)
    (asserts! (is-eq (contract-of token-contract) (get token-contract pool-data)) ERR-POOL-NOT-FOUND)
    
    ;; Transfer yield to user
    (try! (as-contract (contract-call? token-contract transfer net-yield tx-sender CONTRACT-OWNER none)))
    
    ;; Update last claim block
    (map-set user-deposits
      { user: tx-sender, pool-id: pool-id }
      (merge user-data { last-claim-block: block-height })
    )
    
    (ok net-yield)
  )
)

;; Admin functions

(define-public (pause-pool (pool-id uint))
  (let (
    (pool-data (unwrap! (get-pool pool-id) ERR-POOL-NOT-FOUND))
  )
    (asserts! (is-owner) ERR-OWNER-ONLY)
    
    (map-set pools
      { pool-id: pool-id }
      (merge pool-data { is-active: false })
    )
    
    (ok true)
  )
)

(define-public (unpause-pool (pool-id uint))
  (let (
    (pool-data (unwrap! (get-pool pool-id) ERR-POOL-NOT-FOUND))
  )
    (asserts! (is-owner) ERR-OWNER-ONLY)
    
    (map-set pools
      { pool-id: pool-id }
      (merge pool-data { is-active: true })
    )
    
    (ok true)
  )
)

(define-public (set-protocol-fee-rate (new-rate uint))
  (begin
    (asserts! (is-owner) ERR-OWNER-ONLY)
    (asserts! (<= new-rate u1000) ERR-INVALID-AMOUNT) ;; Max 10%
    (var-set protocol-fee-rate new-rate)
    (ok true)
  )
)

(define-public (emergency-withdraw (token-contract <sip-010-trait>) (pool-id uint) (amount uint))
  (let (
    (pool-data (unwrap! (get-pool pool-id) ERR-POOL-NOT-FOUND))
  )
    (asserts! (is-owner) ERR-OWNER-ONLY)
    (asserts! (is-eq (contract-of token-contract) (get token-contract pool-data)) ERR-POOL-NOT-FOUND)
    
    (try! (as-contract (contract-call? token-contract transfer amount tx-sender CONTRACT-OWNER none)))
    
    (ok amount)
  )
)
