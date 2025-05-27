;; Yield Pool Management Contract
;; Manages multiple yield farming pools for different DeFi protocols

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u1000))
(define-constant ERR_POOL_NOT_FOUND (err u1001))
(define-constant ERR_INSUFFICIENT_BALANCE (err u1002))
(define-constant ERR_INVALID_AMOUNT (err u1003))
(define-constant ERR_POOL_ALREADY_EXISTS (err u1004))
(define-constant ERR_ZERO_SHARES (err u1005))
(define-constant ERR_POOL_PAUSED (err u1006))

;; Data Variables
(define-data-var next-pool-id uint u1)

;; Pool structure
(define-map pools
  { pool-id: uint }
  {
    protocol-name: (string-ascii 50),
    asset-contract: principal,
    total-deposited: uint,
    total-shares: uint,
    share-price: uint, ;; Price per share in micro-units
    is-active: bool,
    created-at: uint,
    admin: principal
  }
)

;; User positions in pools
(define-map user-positions
  { user: principal, pool-id: uint }
  {
    deposited-amount: uint,
    shares-owned: uint,
    last-deposit: uint,
    rewards-claimed: uint
  }
)

;; Pool rewards tracking
(define-map pool-rewards
  { pool-id: uint }
  {
    total-rewards: uint,
    reward-rate: uint, ;; Rewards per block per share
    last-update: uint
  }
)

;; User rewards tracking
(define-map user-rewards
  { user: principal, pool-id: uint }
  {
    pending-rewards: uint,
    last-claim: uint
  }
)

;; Pool registry for easy enumeration
(define-map pool-registry
  { index: uint }
  { pool-id: uint }
)

(define-data-var total-pools uint u0)

;; Admin functions

;; Create a new yield pool
(define-public (create-pool (protocol-name (string-ascii 50)) (asset-contract principal))
  (let (
    (pool-id (var-get next-pool-id))
  )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    
    ;; Check if pool already exists for this protocol
    (asserts! (is-none (get-pool-by-protocol protocol-name)) ERR_POOL_ALREADY_EXISTS)
    
    ;; Create the pool
    (map-set pools
      { pool-id: pool-id }
      {
        protocol-name: protocol-name,
        asset-contract: asset-contract,
        total-deposited: u0,
        total-shares: u0,
        share-price: u1000000, ;; Initial price: 1 token = 1 share (in micro-units)
        is-active: true,
        created-at: block-height,
        admin: tx-sender
      }
    )
    
    ;; Initialize pool rewards
    (map-set pool-rewards
      { pool-id: pool-id }
      {
        total-rewards: u0,
        reward-rate: u0,
        last-update: block-height
      }
    )
    
    ;; Add to registry
    (map-set pool-registry
      { index: (var-get total-pools) }
      { pool-id: pool-id }
    )
    
    ;; Update counters
    (var-set next-pool-id (+ pool-id u1))
    (var-set total-pools (+ (var-get total-pools) u1))
    
    (ok pool-id)
  )
)

;; Update pool status
(define-public (toggle-pool-status (pool-id uint))
  (let (
    (pool-data (unwrap! (map-get? pools { pool-id: pool-id }) ERR_POOL_NOT_FOUND))
  )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    
    (map-set pools
      { pool-id: pool-id }
      (merge pool-data { is-active: (not (get is-active pool-data)) })
    )
    
    (ok true)
  )
)

;; User functions

;; Deposit assets into a pool
(define-public (deposit (pool-id uint) (amount uint))
  (let (
    (pool-data (unwrap! (map-get? pools { pool-id: pool-id }) ERR_POOL_NOT_FOUND))
    (current-position (default-to 
      { deposited-amount: u0, shares-owned: u0, last-deposit: u0, rewards-claimed: u0 }
      (map-get? user-positions { user: tx-sender, pool-id: pool-id })
    ))
    (shares-to-mint (calculate-shares-for-deposit amount (get share-price pool-data)))
  )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (get is-active pool-data) ERR_POOL_PAUSED)
    
    ;; Update pool totals
    (map-set pools
      { pool-id: pool-id }
      (merge pool-data {
        total-deposited: (+ (get total-deposited pool-data) amount),
        total-shares: (+ (get total-shares pool-data) shares-to-mint)
      })
    )
    
    ;; Update user position
    (map-set user-positions
      { user: tx-sender, pool-id: pool-id }
      {
        deposited-amount: (+ (get deposited-amount current-position) amount),
        shares-owned: (+ (get shares-owned current-position) shares-to-mint),
        last-deposit: block-height,
        rewards-claimed: (get rewards-claimed current-position)
      }
    )
    
    ;; Initialize user rewards if first deposit
    (if (is-eq (get shares-owned current-position) u0)
      (map-set user-rewards
        { user: tx-sender, pool-id: pool-id }
        { pending-rewards: u0, last-claim: block-height }
      )
      true
    )
    
    (ok shares-to-mint)
  )
)

;; Withdraw assets from a pool
(define-public (withdraw (pool-id uint) (shares-to-burn uint))
  (let (
    (pool-data (unwrap! (map-get? pools { pool-id: pool-id }) ERR_POOL_NOT_FOUND))
    (user-position (unwrap! (map-get? user-positions { user: tx-sender, pool-id: pool-id }) ERR_INSUFFICIENT_BALANCE))
    (withdrawal-amount (calculate-withdrawal-amount shares-to-burn (get share-price pool-data)))
  )
    (asserts! (> shares-to-burn u0) ERR_INVALID_AMOUNT)
    (asserts! (>= (get shares-owned user-position) shares-to-burn) ERR_INSUFFICIENT_BALANCE)
    
    ;; Update pool totals
    (map-set pools
      { pool-id: pool-id }
      (merge pool-data {
        total-deposited: (- (get total-deposited pool-data) withdrawal-amount),
        total-shares: (- (get total-shares pool-data) shares-to-burn)
      })
    )
    
    ;; Update user position
    (map-set user-positions
      { user: tx-sender, pool-id: pool-id }
      (merge user-position {
        deposited-amount: (- (get deposited-amount user-position) withdrawal-amount),
        shares-owned: (- (get shares-owned user-position) shares-to-burn)
      })
    )
    
    (ok withdrawal-amount)
  )
)

;; Claim rewards
(define-public (claim-rewards (pool-id uint))
  (let (
    (user-position (unwrap! (map-get? user-positions { user: tx-sender, pool-id: pool-id }) ERR_POOL_NOT_FOUND))
    (pending-rewards (calculate-pending-rewards tx-sender pool-id))
  )
    (asserts! (> pending-rewards u0) ERR_ZERO_SHARES)
    
    ;; Update user rewards
    (map-set user-rewards
      { user: tx-sender, pool-id: pool-id }
      { pending-rewards: u0, last-claim: block-height }
    )
    
    ;; Update user position
    (map-set user-positions
      { user: tx-sender, pool-id: pool-id }
      (merge user-position {
        rewards-claimed: (+ (get rewards-claimed user-position) pending-rewards)
      })
    )
    
    (ok pending-rewards)
  )
)

;; Read-only functions

;; Get pool information
(define-read-only (get-pool-info (pool-id uint))
  (map-get? pools { pool-id: pool-id })
)

;; Get user position
(define-read-only (get-user-position (user principal) (pool-id uint))
  (map-get? user-positions { user: user, pool-id: pool-id })
)

;; Get user rewards
(define-read-only (get-user-rewards (user principal) (pool-id uint))
  (map-get? user-rewards { user: user, pool-id: pool-id })
)

;; Get total number of pools
(define-read-only (get-total-pools)
  (var-get total-pools)
)

;; Get pool by index
(define-read-only (get-pool-by-index (index uint))
  (match (map-get? pool-registry { index: index })
    registry-entry (map-get? pools { pool-id: (get pool-id registry-entry) })
    none
  )
)

;; Helper functions

;; Calculate shares for deposit
(define-private (calculate-shares-for-deposit (amount uint) (share-price uint))
  (/ (* amount u1000000) share-price)
)

;; Calculate withdrawal amount
(define-private (calculate-withdrawal-amount (shares uint) (share-price uint))
  (/ (* shares share-price) u1000000)
)

;; Calculate pending rewards for a user
(define-private (calculate-pending-rewards (user principal) (pool-id uint))
  (let (
    (user-position (default-to 
      { deposited-amount: u0, shares-owned: u0, last-deposit: u0, rewards-claimed: u0 }
      (map-get? user-positions { user: user, pool-id: pool-id })
    ))
    (pool-rewards-data (default-to
      { total-rewards: u0, reward-rate: u0, last-update: u0 }
      (map-get? pool-rewards { pool-id: pool-id })
    ))
    (user-rewards-data (default-to
      { pending-rewards: u0, last-claim: u0 }
      (map-get? user-rewards { user: user, pool-id: pool-id })
    ))
    (blocks-since-claim (- block-height (get last-claim user-rewards-data)))
    (reward-per-share (get reward-rate pool-rewards-data))
    (user-shares (get shares-owned user-position))
  )
    (+ (get pending-rewards user-rewards-data)
       (/ (* user-shares reward-per-share blocks-since-claim) u1000000))
  )
)

;; Get pool by protocol name
(define-private (get-pool-by-protocol (protocol-name (string-ascii 50)))
  (let (
    (total (var-get total-pools))
  )
    (find-pool-by-protocol protocol-name u0 total)
  )
)

;; Helper to find pool by protocol (recursive)
(define-private (find-pool-by-protocol (protocol-name (string-ascii 50)) (index uint) (max-index uint))
  (if (>= index max-index)
    none
    (match (get-pool-by-index index)
      pool-data (if (is-eq (get protocol-name pool-data) protocol-name)
                   (some pool-data)
                   (find-pool-by-protocol protocol-name (+ index u1) max-index))
      (find-pool-by-protocol protocol-name (+ index u1) max-index)
    )
  )
)

;; Administrative functions for reward management

;; Add rewards to a pool
(define-public (add-pool-rewards (pool-id uint) (reward-amount uint))
  (let (
    (pool-data (unwrap! (map-get? pools { pool-id: pool-id }) ERR_POOL_NOT_FOUND))
    (current-rewards (default-to
      { total-rewards: u0, reward-rate: u0, last-update: u0 }
      (map-get? pool-rewards { pool-id: pool-id })
    ))
  )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    
    (map-set pool-rewards
      { pool-id: pool-id }
      (merge current-rewards {
        total-rewards: (+ (get total-rewards current-rewards) reward-amount),
        last-update: block-height
      })
    )
    
    (ok true)
  )
)

;; Set reward rate for a pool
(define-public (set-reward-rate (pool-id uint) (rate uint))
  (let (
    (pool-data (unwrap! (map-get? pools { pool-id: pool-id }) ERR_POOL_NOT_FOUND))
    (current-rewards (default-to
      { total-rewards: u0, reward-rate: u0, last-update: u0 }
      (map-get? pool-rewards { pool-id: pool-id })
    ))
  )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    
    (map-set pool-rewards
      { pool-id: pool-id }
      (merge current-rewards {
        reward-rate: rate,
        last-update: block-height
      })
    )
    
    (ok true)
  )
)