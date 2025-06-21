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
(define-constant ERR_DIVISION_BY_ZERO (err u1007))
(define-constant ERR_OVERFLOW (err u1008))

;; Data Variables
(define-data-var next-pool-id uint u1)
(define-data-var total-pools uint u0)

;; Pool structure
(define-map pools
  { pool-id: uint }
  {
    protocol-name: (string-ascii 50),
    asset-contract: principal,
    total-deposited: uint,
    total-shares: uint,
    share-price: uint, ;; Price per share in micro-units (1e6 = 1 token)
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
    reward-rate: uint, ;; Rewards per block per million shares
    last-update: uint,
    rewards-per-share-stored: uint
  }
)

;; User rewards tracking
(define-map user-rewards
  { user: principal, pool-id: uint }
  {
    rewards-per-share-paid: uint,
    pending-rewards: uint,
    last-claim: uint
  }
)

;; Pool registry for enumeration
(define-map pool-registry
  { index: uint }
  { pool-id: uint }
)

;; Protocol name to pool ID mapping
(define-map protocol-to-pool
  { protocol-name: (string-ascii 50) }
  { pool-id: uint }
)

;; Admin functions

;; Create a new yield pool
(define-public (create-pool (protocol-name (string-ascii 50)) (asset-contract principal))
  (let (
    (pool-id (var-get next-pool-id))
    (current-total (var-get total-pools))
  )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> (len protocol-name) u0) ERR_INVALID_AMOUNT)
    
    ;; Check if pool already exists for this protocol
    (asserts! (is-none (map-get? protocol-to-pool { protocol-name: protocol-name })) ERR_POOL_ALREADY_EXISTS)
    
    ;; Create the pool
    (map-set pools
      { pool-id: pool-id }
      {
        protocol-name: protocol-name,
        asset-contract: asset-contract,
        total-deposited: u0,
        total-shares: u0,
        share-price: u1000000, ;; Initial price: 1 token = 1 share (1e6 micro-units)
        is-active: true,
        created-at: stacks-block-height,
        admin: tx-sender
      }
    )
    
    ;; Initialize pool rewards
    (map-set pool-rewards
      { pool-id: pool-id }
      {
        total-rewards: u0,
        reward-rate: u0,
        last-update: stacks-block-height,
        rewards-per-share-stored: u0
      }
    )
    
    ;; Add to protocol mapping
    (map-set protocol-to-pool
      { protocol-name: protocol-name }
      { pool-id: pool-id }
    )
    
    ;; Add to registry
    (map-set pool-registry
      { index: current-total }
      { pool-id: pool-id }
    )
    
    ;; Update counters
    (var-set next-pool-id (+ pool-id u1))
    (var-set total-pools (+ current-total u1))
    
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
    
    (ok (not (get is-active pool-data)))
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
    (shares-to-mint (try! (calculate-shares-for-deposit amount (get share-price pool-data))))
    (new-total-deposited (+ (get total-deposited pool-data) amount))
    (new-total-shares (+ (get total-shares pool-data) shares-to-mint))
  )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (get is-active pool-data) ERR_POOL_PAUSED)
    
    ;; Update rewards before changing user's share balance
    (try! (update-user-rewards tx-sender pool-id))
    
    ;; Update pool totals
    (map-set pools
      { pool-id: pool-id }
      (merge pool-data {
        total-deposited: new-total-deposited,
        total-shares: new-total-shares
      })
    )
    
    ;; Update user position
    (map-set user-positions
      { user: tx-sender, pool-id: pool-id }
      {
        deposited-amount: (+ (get deposited-amount current-position) amount),
        shares-owned: (+ (get shares-owned current-position) shares-to-mint),
        last-deposit: stacks-block-height,
        rewards-claimed: (get rewards-claimed current-position)
      }
    )
    
    (ok shares-to-mint)
  )
)

;; Withdraw assets from a pool
(define-public (withdraw (pool-id uint) (shares-to-burn uint))
  (let (
    (pool-data (unwrap! (map-get? pools { pool-id: pool-id }) ERR_POOL_NOT_FOUND))
    (user-position (unwrap! (map-get? user-positions { user: tx-sender, pool-id: pool-id }) ERR_INSUFFICIENT_BALANCE))
    (withdrawal-amount (try! (calculate-withdrawal-amount shares-to-burn (get share-price pool-data))))
    (new-total-deposited (- (get total-deposited pool-data) withdrawal-amount))
    (new-total-shares (- (get total-shares pool-data) shares-to-burn))
  )
    (asserts! (> shares-to-burn u0) ERR_INVALID_AMOUNT)
    (asserts! (>= (get shares-owned user-position) shares-to-burn) ERR_INSUFFICIENT_BALANCE)
    (asserts! (>= (get total-deposited pool-data) withdrawal-amount) ERR_INSUFFICIENT_BALANCE)
    
    ;; Update rewards before changing user's share balance
    (try! (update-user-rewards tx-sender pool-id))
    
    ;; Update pool totals
    (map-set pools
      { pool-id: pool-id }
      (merge pool-data {
        total-deposited: new-total-deposited,
        total-shares: new-total-shares
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
  )
    ;; Update rewards calculation
    (try! (update-user-rewards tx-sender pool-id))
    
    (let (
      (user-rewards-data (unwrap! (map-get? user-rewards { user: tx-sender, pool-id: pool-id }) ERR_POOL_NOT_FOUND))
      (pending-rewards (get pending-rewards user-rewards-data))
    )
      (asserts! (> pending-rewards u0) ERR_ZERO_SHARES)
      
      ;; Reset user rewards
      (map-set user-rewards
        { user: tx-sender, pool-id: pool-id }
        (merge user-rewards-data {
          pending-rewards: u0,
          last-claim: stacks-block-height
        })
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

;; Get pending rewards for a user
(define-read-only (get-pending-rewards (user principal) (pool-id uint))
  (match (calculate-pending-rewards user pool-id)
    success (ok success)
    error (err error)
  )
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

;; Get pool by protocol name
(define-read-only (get-pool-by-protocol (protocol-name (string-ascii 50)))
  (match (map-get? protocol-to-pool { protocol-name: protocol-name })
    mapping (map-get? pools { pool-id: (get pool-id mapping) })
    none
  )
)

;; Helper functions

;; Calculate shares for deposit with overflow protection
(define-private (calculate-shares-for-deposit (amount uint) (share-price uint))
  (begin
    (asserts! (> share-price u0) ERR_DIVISION_BY_ZERO)
    (let (
      (numerator (* amount u1000000))
    )
      ;; Check for overflow
      (asserts! (>= numerator amount) ERR_OVERFLOW)
      (ok (/ numerator share-price))
    )
  )
)

;; Calculate withdrawal amount with overflow protection
(define-private (calculate-withdrawal-amount (shares uint) (share-price uint))
  (let (
    (numerator (* shares share-price))
  )
    ;; Check for overflow
    (asserts! (>= numerator shares) ERR_OVERFLOW)
    (ok (/ numerator u1000000))
  )
)

;; Helper function to safely calculate earned rewards
(define-private (safe-calculate-earned-rewards (user-shares uint) (rewards-per-share uint) (rewards-per-share-paid uint))
  (if (or 
    (< rewards-per-share rewards-per-share-paid)
    (is-eq user-shares u0)
  )
    u0
    (let (
      (reward-diff (- rewards-per-share rewards-per-share-paid))
    )
      (if (is-eq reward-diff u0)
        u0
        (let (
          (earned (* user-shares reward-diff))
        )
          (if (< earned user-shares)
            u0  ;; Return 0 on overflow instead of error
            (/ earned u1000000)
          )
        )
      )
    )
  )
)

;; Update user rewards calculation - SIMPLE VERSION
(define-private (update-user-rewards (user principal) (pool-id uint))
  (match (calculate-rewards-per-share pool-id)
    current-rewards-per-share
    (let (
      (pool-rewards-data (default-to
        { total-rewards: u0, reward-rate: u0, last-update: u0, rewards-per-share-stored: u0 }
        (map-get? pool-rewards { pool-id: pool-id })
      ))
      (user-position (default-to
        { deposited-amount: u0, shares-owned: u0, last-deposit: u0, rewards-claimed: u0 }
        (map-get? user-positions { user: user, pool-id: pool-id })
      ))
      (user-rewards-data (default-to
        { rewards-per-share-paid: u0, pending-rewards: u0, last-claim: u0 }
        (map-get? user-rewards { user: user, pool-id: pool-id })
      ))
      (earned-rewards (safe-calculate-earned-rewards
        (get shares-owned user-position)
        current-rewards-per-share
        (get rewards-per-share-paid user-rewards-data)
      ))
    )
      ;; Update pool rewards per share
      (map-set pool-rewards
        { pool-id: pool-id }
        (merge pool-rewards-data {
          rewards-per-share-stored: current-rewards-per-share,
          last-update: stacks-block-height
        })
      )
      ;; Update user rewards
      (map-set user-rewards
        { user: user, pool-id: pool-id }
        {
          rewards-per-share-paid: current-rewards-per-share,
          pending-rewards: (+ (get pending-rewards user-rewards-data) earned-rewards),
          last-claim: (get last-claim user-rewards-data)
        }
      )
      (ok true)
    )
    error-code (err error-code)
  )
)


;; Calculate rewards per share
(define-private (calculate-rewards-per-share (pool-id uint))
  (let (
    (pool-data (unwrap! (map-get? pools { pool-id: pool-id }) ERR_POOL_NOT_FOUND))
    (pool-rewards-data (default-to
      { total-rewards: u0, reward-rate: u0, last-update: u0, rewards-per-share-stored: u0 }
      (map-get? pool-rewards { pool-id: pool-id })
    ))
    (total-shares (get total-shares pool-data))
  )
    (if (is-eq total-shares u0)
      (ok (get rewards-per-share-stored pool-rewards-data))
      (let (
        (blocks-elapsed (- stacks-block-height (get last-update pool-rewards-data)))
        (reward-increment (/ (* (get reward-rate pool-rewards-data) blocks-elapsed u1000000) total-shares))
      )
        (ok (+ (get rewards-per-share-stored pool-rewards-data) reward-increment))
      )
    )
  )
)

;; Calculate earned rewards for a user
(define-private (calculate-earned-rewards (user-shares uint) (rewards-per-share uint) (rewards-per-share-paid uint))
  (if (>= rewards-per-share rewards-per-share-paid)
    (let (
      (reward-diff (- rewards-per-share rewards-per-share-paid))
      (earned (* user-shares reward-diff))
    )
      ;; Check for overflow
      (if (and (> user-shares u0) (> reward-diff u0))
        (if (>= earned user-shares) ;; Simple overflow check
          (ok (/ earned u1000000))
          (err ERR_OVERFLOW))
        (ok u0)
      )
    )
    (ok u0) ;; If rewards-per-share-paid is somehow higher, return 0
  )
)

;; Also update your calculate-pending-rewards function to handle the response type:
(define-private (calculate-pending-rewards (user principal) (pool-id uint))
  (let (
    (user-rewards-data (default-to
      { rewards-per-share-paid: u0, pending-rewards: u0, last-claim: u0 }
      (map-get? user-rewards { user: user, pool-id: pool-id })
    ))
    (user-position (default-to 
      { deposited-amount: u0, shares-owned: u0, last-deposit: u0, rewards-claimed: u0 }
      (map-get? user-positions { user: user, pool-id: pool-id })
    ))
    (current-rewards-per-share (try! (calculate-rewards-per-share pool-id)))
    (earned-rewards (unwrap! (calculate-earned-rewards 
      (get shares-owned user-position)
      current-rewards-per-share
      (get rewards-per-share-paid user-rewards-data)
    ) (err u404)))
    (total-pending (+ (get pending-rewards user-rewards-data) earned-rewards))
  )
    (ok total-pending)
  )
)

;; Administrative functions for reward management

;; Add rewards to a pool
(define-public (add-pool-rewards (pool-id uint) (reward-amount uint))
  (let (
    (pool-data (unwrap! (map-get? pools { pool-id: pool-id }) ERR_POOL_NOT_FOUND))
    (current-rewards (default-to
      { total-rewards: u0, reward-rate: u0, last-update: u0, rewards-per-share-stored: u0 }
      (map-get? pool-rewards { pool-id: pool-id })
    ))
  )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> reward-amount u0) ERR_INVALID_AMOUNT)
    
    (map-set pool-rewards
      { pool-id: pool-id }
      (merge current-rewards {
        total-rewards: (+ (get total-rewards current-rewards) reward-amount),
        last-update: stacks-block-height
      })
    )
    
    (ok true)
  )
)

;; Set reward rate for a pool (rewards per block per million shares)
(define-public (set-reward-rate (pool-id uint) (rate uint))
  (let (
    (pool-data (unwrap! (map-get? pools { pool-id: pool-id }) ERR_POOL_NOT_FOUND))
    (current-rewards (default-to
      { total-rewards: u0, reward-rate: u0, last-update: u0, rewards-per-share-stored: u0 }
      (map-get? pool-rewards { pool-id: pool-id })
    ))
  )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    
    ;; Update rewards before changing rate
    (try! (calculate-rewards-per-share pool-id))
    
    (map-set pool-rewards
      { pool-id: pool-id }
      (merge current-rewards {
        reward-rate: rate,
        last-update: stacks-block-height
      })
    )
    
    (ok true)
  )
)