;; Yield Pool Smart Contract
;; Handles user deposits and withdrawals with yield distribution

;; ;; Import SIP-010 trait
;; (use-trait sip-010-trait 'SP3FBR2AGK5H9QBDH3EEN6DF8EK8JY7RX8QJ5SVTE.sip-010-trait)
;; (impl-trait 'SP3FBR2AGK5H9QBDH3EEN6DF8EK8JY7RX8QJ5SVTE.sip-010-trait)

(define-trait sip-010-trait
  (
    ;; Transfer tokens from one principal to another
    (transfer (uint principal principal (optional principal)) (response bool uint))

    ;; Get the balance of a principal
    (get-balance (principal) (response uint uint))

    ;; Get the total supply of the token
    (get-total-supply () (response uint uint))

    ;; Get the decimals of the token
    (get-decimals () (response uint uint))

    ;; Get the name of the token
    (get-name () (response (string-ascii 32) uint))

    ;; Get the symbol of the token
    (get-symbol () (response (string-ascii 32) uint))
  )
)

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-OWNER-ONLY (err u100))
(define-constant ERR-NOT-AUTHORIZED (err u101))
(define-constant ERR-INVALID-AMOUNT (err u102))
(define-constant ERR-INSUFFICIENT-BALANCE (err u103))
(define-constant ERR-POOL-NOT-FOUND (err u104))
(define-constant ERR-INSUFFICIENT-SHARES (err u105))
(define-constant ERR-POOL-PAUSED (err u106))
(define-constant ERR-TRANSFER-FAILED (err u107))
(define-constant ERR-ZERO-SHARES (err u108))

;; Data Variables
(define-data-var contract-paused bool false)
(define-data-var pool-counter uint u0)

;; Data Maps
(define-map pools
  { pool-id: uint }
  {
    token-contract: principal,
    total-deposits: uint,
    total-shares: uint,
    yield-rate: uint, ;; Annual yield rate in basis points (e.g., 500 = 5%)
    last-yield-update: uint,
    is-active: bool,
    min-deposit: uint,
    pool-name: (string-ascii 50)
  }
)

(define-map user-positions
  { user: principal, pool-id: uint }
  {
    shares: uint,
    deposit-time: uint,
    last-claim-time: uint
  }
)

(define-map pool-admins
  { pool-id: uint, admin: principal }
  { authorized: bool }
)

;; Read-only functions

(define-read-only (get-pool-info (pool-id uint))
  (map-get? pools { pool-id: pool-id })
)

(define-read-only (get-user-position (user principal) (pool-id uint))
  (map-get? user-positions { user: user, pool-id: pool-id })
)

(define-read-only (get-pool-count)
  (var-get pool-counter)
)

(define-read-only (is-contract-paused)
  (var-get contract-paused)
)

(define-read-only (calculate-share-value (pool-id uint) (shares uint))
  (let (
    (pool-data (unwrap! (get-pool-info pool-id) (err ERR-POOL-NOT-FOUND)))
    (total-shares (get total-shares pool-data))
    (total-deposits (get total-deposits pool-data))
  )
    (if (is-eq total-shares u0)
      (ok u0)
      (ok (/ (* shares total-deposits) total-shares))
    )
  )
)

(define-read-only (calculate-yield (user principal) (pool-id uint))
  (let (
    (position (unwrap! (get-user-position user pool-id) (err ERR-POOL-NOT-FOUND)))
    (pool-data (unwrap! (get-pool-info pool-id) (err ERR-POOL-NOT-FOUND)))
    (shares (get shares position))
    (deposit-time (get deposit-time position))
    (last-claim-time (get last-claim-time position))
    (yield-rate (get yield-rate pool-data))
    (current-time block-height)
    (time-elapsed (- current-time last-claim-time))
  )
    (if (is-eq shares u0)
      (ok u0)
      (let (
        (share-value (unwrap! (calculate-share-value pool-id shares) (err ERR-POOL-NOT-FOUND)))
        ;; Simple yield calculation: (principal * rate * time) / (365 * 24 * 6 * 10000)
        ;; Assuming 6 blocks per hour, 24 hours per day, 365 days per year
        (yield-amount (/ (* (* share-value yield-rate) time-elapsed) u525600000))
      )
        (ok yield-amount)
      )
    )
  )
)

;; Private functions

(define-private (is-pool-admin (pool-id uint) (user principal))
  (default-to false (get authorized (map-get? pool-admins { pool-id: pool-id, admin: user })))
)

;; Public functions

(define-public (create-pool 
  (token-contract <sip-010-trait>)
  (yield-rate uint)
  (min-deposit uint)
  (pool-name (string-ascii 50))
)
  (let (
    (new-pool-id (+ (var-get pool-counter) u1))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (asserts! (not (var-get contract-paused)) ERR-POOL-PAUSED)
    (asserts! (> yield-rate u0) ERR-INVALID-AMOUNT)
    
    (map-set pools
      { pool-id: new-pool-id }
      {
        token-contract: (contract-of token-contract),
        total-deposits: u0,
        total-shares: u0,
        yield-rate: yield-rate,
        last-yield-update: block-height,
        is-active: true,
        min-deposit: min-deposit,
        pool-name: pool-name
      }
    )
    
    (map-set pool-admins
      { pool-id: new-pool-id, admin: CONTRACT-OWNER }
      { authorized: true }
    )
    
    (var-set pool-counter new-pool-id)
    
    (print {
      event: "pool-created",
      pool-id: new-pool-id,
      token-contract: (contract-of token-contract),
      yield-rate: yield-rate,
      creator: tx-sender
    })
    
    (ok new-pool-id)
  )
)

(define-public (deposit (pool-id uint) (amount uint) (token-contract <sip-010-trait>))
  (let (
    (pool-data (unwrap! (get-pool-info pool-id) ERR-POOL-NOT-FOUND))
    (stored-token-contract (get token-contract pool-data))
    (total-deposits (get total-deposits pool-data))
    (total-shares (get total-shares pool-data))
    (min-deposit (get min-deposit pool-data))
    (current-position (default-to 
      { shares: u0, deposit-time: block-height, last-claim-time: block-height }
      (get-user-position tx-sender pool-id)
    ))
    (shares-to-mint (if (is-eq total-deposits u0)
      amount ;; First deposit: 1:1 ratio
      (/ (* amount total-shares) total-deposits) ;; Subsequent deposits: proportional
    ))
  )
    (asserts! (not (var-get contract-paused)) ERR-POOL-PAUSED)
    (asserts! (get is-active pool-data) ERR-POOL-NOT-FOUND)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (>= amount min-deposit) ERR-INVALID-AMOUNT)
    (asserts! (> shares-to-mint u0) ERR-ZERO-SHARES)
    (asserts! (is-eq (contract-of token-contract) stored-token-contract) ERR-NOT-AUTHORIZED)
    
    ;; Transfer tokens from user to contract
    (unwrap! (contract-call? token-contract transfer 
      amount tx-sender (as-contract tx-sender) none) ERR-TRANSFER-FAILED)
    
    ;; Update pool data
    (map-set pools
      { pool-id: pool-id }
      (merge pool-data {
        total-deposits: (+ total-deposits amount),
        total-shares: (+ total-shares shares-to-mint)
      })
    )
    
    ;; Update user position
    (map-set user-positions
      { user: tx-sender, pool-id: pool-id }
      {
        shares: (+ (get shares current-position) shares-to-mint),
        deposit-time: (if (is-eq (get shares current-position) u0) 
          block-height 
          (get deposit-time current-position)),
        last-claim-time: block-height
      }
    )
    
    (print {
      event: "deposit",
      user: tx-sender,
      pool-id: pool-id,
      amount: amount,
      shares-minted: shares-to-mint
    })
    
    (ok shares-to-mint)
  )
)
(define-public (withdraw (pool-id uint) (shares-to-burn uint) (token-contract <sip-010-trait>))
  (let (
    (pool-data (unwrap! (get-pool-info pool-id) ERR-POOL-NOT-FOUND))
    (stored-token-contract (get token-contract pool-data))
    (total-deposits (get total-deposits pool-data))
    (total-shares (get total-shares pool-data))
    (user-position (unwrap! (get-user-position tx-sender pool-id) ERR-POOL-NOT-FOUND))
    (user-shares (get shares user-position))
    (withdrawal-amount (unwrap! (calculate-share-value pool-id shares-to-burn) ERR-POOL-NOT-FOUND))
    (yield-amount (unwrap! (calculate-yield tx-sender pool-id) ERR-POOL-NOT-FOUND))
    (total-withdrawal (+ withdrawal-amount yield-amount))
  )
    (asserts! (not (var-get contract-paused)) ERR-POOL-PAUSED)
    (asserts! (get is-active pool-data) ERR-POOL-NOT-FOUND)
    (asserts! (> shares-to-burn u0) ERR-INVALID-AMOUNT)
    (asserts! (>= user-shares shares-to-burn) ERR-INSUFFICIENT-SHARES)
    (asserts! (>= total-deposits total-withdrawal) ERR-INSUFFICIENT-BALANCE)
    (asserts! (is-eq (contract-of token-contract) stored-token-contract) ERR-NOT-AUTHORIZED)
    
    ;; Transfer tokens from contract to user
    ;; SIP-010 transfer function requires: (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34)))
    (try! (as-contract (contract-call? token-contract transfer 
      total-withdrawal 
      (as-contract tx-sender) 
      tx-sender 
      none)))
    
    ;; Update pool data
    (map-set pools
      { pool-id: pool-id }
      (merge pool-data {
        total-deposits: (- total-deposits withdrawal-amount),
        total-shares: (- total-shares shares-to-burn)
      })
    )
    
    ;; Update user position
    (let ((remaining-shares (- user-shares shares-to-burn)))
      (if (is-eq remaining-shares u0)
        (map-delete user-positions { user: tx-sender, pool-id: pool-id })
        (map-set user-positions
          { user: tx-sender, pool-id: pool-id }
          (merge user-position {
            shares: remaining-shares,
            last-claim-time: block-height
          })
        )
      )
    )
    
    (print {
      event: "withdrawal",
      user: tx-sender,
      pool-id: pool-id,
      shares-burned: shares-to-burn,
      principal-amount: withdrawal-amount,
      yield-amount: yield-amount,
      total-amount: total-withdrawal
    })
    
    (ok total-withdrawal)
  )
)

;; (define-public (withdraw (pool-id uint) (shares-to-burn uint) (token-contract <sip-010-trait>))
;;   (let (
;;     (pool-data (unwrap! (get-pool-info pool-id) ERR-POOL-NOT-FOUND))
;;     (stored-token-contract (get token-contract pool-data))
;;     (total-deposits (get total-deposits pool-data))
;;     (total-shares (get total-shares pool-data))
;;     (user-position (unwrap! (get-user-position tx-sender pool-id) ERR-POOL-NOT-FOUND))
;;     (user-shares (get shares user-position))
;;     (withdrawal-amount (unwrap! (calculate-share-value pool-id shares-to-burn) ERR-POOL-NOT-FOUND))
;;     (yield-amount (unwrap! (calculate-yield tx-sender pool-id) ERR-POOL-NOT-FOUND))
;;     (total-withdrawal (+ withdrawal-amount yield-amount))
;;   )
;;     (asserts! (not (var-get contract-paused)) ERR-POOL-PAUSED)
;;     (asserts! (get is-active pool-data) ERR-POOL-NOT-FOUND)
;;     (asserts! (> shares-to-burn u0) ERR-INVALID-AMOUNT)
;;     (asserts! (>= user-shares shares-to-burn) ERR-INSUFFICIENT-SHARES)
;;     (asserts! (>= total-deposits total-withdrawal) ERR-INSUFFICIENT-BALANCE)
;;     (asserts! (is-eq (contract-of token-contract) stored-token-contract) ERR-NOT-AUTHORIZED)
    
;;     ;; Transfer tokens from contract to user
;;     (unwrap! (as-contract (contract-call? token-contract transfer 
;;       total-withdrawal tx-sender)) (err ERR-TRANSFER-FAILED))
    
;;     ;; Update pool data
;;     (map-set pools
;;       { pool-id: pool-id }
;;       (merge pool-data {
;;         total-deposits: (- total-deposits withdrawal-amount),
;;         total-shares: (- total-shares shares-to-burn)
;;       })
;;     )
    
;;     ;; Update user position
;;     (let ((remaining-shares (- user-shares shares-to-burn)))
;;       (if (is-eq remaining-shares u0)
;;         (map-delete user-positions { user: tx-sender, pool-id: pool-id })
;;         (map-set user-positions
;;           { user: tx-sender, pool-id: pool-id }
;;           (merge user-position {
;;             shares: remaining-shares,
;;             last-claim-time: block-height
;;           })
;;         )
;;       )
;;     )
    
;;     (print {
;;       event: "withdrawal",
;;       user: tx-sender,
;;       pool-id: pool-id,
;;       shares-burned: shares-to-burn,
;;       principal-amount: withdrawal-amount,
;;       yield-amount: yield-amount,
;;       total-amount: total-withdrawal
;;     })
    
;;     (ok total-withdrawal)
;;   )
;; )

(define-public (claim-yield (pool-id uint) (token-contract <sip-010-trait>))
  (let (
    (pool-data (unwrap! (get-pool-info pool-id) ERR-POOL-NOT-FOUND))
    (stored-token-contract (get token-contract pool-data))
    (user-position (unwrap! (get-user-position tx-sender pool-id) ERR-POOL-NOT-FOUND))
    (yield-amount (unwrap! (calculate-yield tx-sender pool-id) ERR-POOL-NOT-FOUND))
  )
    (asserts! (not (var-get contract-paused)) ERR-POOL-PAUSED)
    (asserts! (get is-active pool-data) ERR-POOL-NOT-FOUND)
    (asserts! (> yield-amount u0) ERR-INVALID-AMOUNT)
    (asserts! (is-eq (contract-of token-contract) stored-token-contract) ERR-NOT-AUTHORIZED)
    
    ;; Transfer yield to user
    (unwrap! (as-contract (contract-call? token-contract transfer 
      yield-amount tx-sender tx-sender none)) ERR-TRANSFER-FAILED)
    
    ;; Update user's last claim time
    (map-set user-positions
      { user: tx-sender, pool-id: pool-id }
      (merge user-position { last-claim-time: block-height })
    )
    
    (print {
      event: "yield-claimed",
      user: tx-sender,
      pool-id: pool-id,
      yield-amount: yield-amount
    })
    
    (ok yield-amount)
  )
)

;; Admin functions

(define-public (pause-contract)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (var-set contract-paused true)
    (print { event: "contract-paused", admin: tx-sender })
    (ok true)
  )
)

(define-public (unpause-contract)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (var-set contract-paused false)
    (print { event: "contract-unpaused", admin: tx-sender })
    (ok true)
  )
)

(define-public (update-yield-rate (pool-id uint) (new-rate uint))
  (let (
    (pool-data (unwrap! (get-pool-info pool-id) ERR-POOL-NOT-FOUND))
  )
    (asserts! (or (is-eq tx-sender CONTRACT-OWNER) 
                  (is-pool-admin pool-id tx-sender)) ERR-NOT-AUTHORIZED)
    (asserts! (> new-rate u0) ERR-INVALID-AMOUNT)
    
    (map-set pools
      { pool-id: pool-id }
      (merge pool-data {
        yield-rate: new-rate,
        last-yield-update: block-height
      })
    )
    
    (print {
      event: "yield-rate-updated",
      pool-id: pool-id,
      new-rate: new-rate,
      admin: tx-sender
    })
    
    (ok true)
  )
)

(define-public (add-pool-admin (pool-id uint) (admin principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (map-set pool-admins
      { pool-id: pool-id, admin: admin }
      { authorized: true }
    )
    (print { event: "admin-added", pool-id: pool-id, admin: admin })
    (ok true)
  )
)

(define-public (remove-pool-admin (pool-id uint) (admin principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (map-delete pool-admins { pool-id: pool-id, admin: admin })
    (print { event: "admin-removed", pool-id: pool-id, admin: admin })
    (ok true)
  )
)

;; Emergency functions

(define-public (emergency-withdraw (pool-id uint) (token-contract <sip-010-trait>) (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (unwrap! (as-contract (contract-call? token-contract transfer 
      amount tx-sender CONTRACT-OWNER none)) ERR-TRANSFER-FAILED)
    (print { event: "emergency-withdrawal", amount: amount, admin: tx-sender })
    (ok true)
  )
)