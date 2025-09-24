;; Reward Distribution Contract
;; Handles accrual and distribution of rewards from multiple farming/staking strategies

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_AMOUNT (err u101))
(define-constant ERR_INSUFFICIENT_BALANCE (err u102))
(define-constant ERR_STRATEGY_NOT_FOUND (err u103))
(define-constant ERR_TOKEN_NOT_SUPPORTED (err u104))
(define-constant ERR_ALREADY_CLAIMED (err u105))
(define-constant ERR_NO_REWARDS (err u106))
(define-constant ERR_INVALID_STRATEGY (err u107))

;; Data Variables
(define-data-var contract-owner principal CONTRACT_OWNER)
(define-data-var total-strategies uint u0)
(define-data-var reward-epoch uint u0)

;; Data Maps

;; Strategy information
(define-map strategies
  { strategy-id: uint }
  {
    name: (string-ascii 50),
    active: bool,
    total-staked: uint,
    reward-rate: uint, ;; rewards per block per unit staked
    last-update-block: uint,
    accumulated-reward-per-share: uint
  }
)

;; Supported reward tokens
(define-map reward-tokens
  { token-contract: principal }
  {
    active: bool,
    decimals: uint,
    total-distributed: uint
  }
)

;; User stakes in strategies
(define-map user-stakes
  { user: principal, strategy-id: uint }
  {
    amount: uint,
    reward-debt: uint, ;; accumulated rewards already accounted for
    last-claim-block: uint
  }
)

;; User rewards per token
(define-map user-rewards
  { user: principal, token-contract: principal, epoch: uint }
  {
    amount: uint,
    claimed: bool
  }
)

;; Strategy reward allocations per token
(define-map strategy-token-rewards
  { strategy-id: uint, token-contract: principal }
  {
    total-allocated: uint,
    distributed: uint,
    rate-per-block: uint
  }
)

;; Read-only functions

(define-read-only (get-contract-owner)
  (var-get contract-owner)
)

(define-read-only (get-strategy (strategy-id uint))
  (map-get? strategies { strategy-id: strategy-id })
)

(define-read-only (get-user-stake (user principal) (strategy-id uint))
  (map-get? user-stakes { user: user, strategy-id: strategy-id })
)

(define-read-only (get-user-rewards (user principal) (token-contract principal) (epoch uint))
  (map-get? user-rewards { user: user, token-contract: token-contract, epoch: epoch })
)

(define-read-only (is-token-supported (token-contract principal))
  (match (map-get? reward-tokens { token-contract: token-contract })
    token-info (get active token-info)
    false
  )
)

;; (define-read-only (calculate-pending-rewards (user principal) (strategy-id uint))
;;   (let (
;;     (strategy (unwrap! (get-strategy strategy-id) ERR_STRATEGY_NOT_FOUND))
;;     (user-stake (unwrap! (get-user-stake user strategy-id) ERR_NO_REWARDS))
;;     (blocks-passed (- block-height (get last-update-block strategy)))
;;     (new-rewards (* blocks-passed (get reward-rate strategy)))
;;     (new-accumulated (+ (get accumulated-reward-per-share strategy) 
;;                        (if (> (get total-staked strategy) u0)
;;                            (/ (* new-rewards u1000000) (get total-staked strategy))
;;                            u0)))
;;     (user-rewards (- (* (get amount user-stake) new-accumulated) 
;;                      (* (get reward-debt user-stake) u1000000)))
;;   )
;;     (ok (/ user-rewards u1000000))
;;   )
;; )

;; Private functions

(define-private (update-strategy-rewards (strategy-id uint))
  (let (
    (strategy (unwrap! (get-strategy strategy-id) ERR_STRATEGY_NOT_FOUND))
    (blocks-passed (- block-height (get last-update-block strategy)))
    (new-rewards (* blocks-passed (get reward-rate strategy)))
    (new-accumulated (+ (get accumulated-reward-per-share strategy) 
                       (if (> (get total-staked strategy) u0)
                           (/ (* new-rewards u1000000) (get total-staked strategy))
                           u0)))
  )
    (map-set strategies
      { strategy-id: strategy-id }
      (merge strategy {
        last-update-block: block-height,
        accumulated-reward-per-share: new-accumulated
      })
    )
    (ok true)
  )
)

;; Public functions

;; Admin functions
(define-public (set-contract-owner (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (var-set contract-owner new-owner)
    (ok true)
  )
)

(define-public (add-reward-token (token-contract principal) (decimals uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (map-set reward-tokens
      { token-contract: token-contract }
      {
        active: true,
        decimals: decimals,
        total-distributed: u0
      }
    )
    (ok true)
  )
)

(define-public (create-strategy (name (string-ascii 50)) (reward-rate uint))
  (let (
    (strategy-id (+ (var-get total-strategies) u1))
  )
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (asserts! (> reward-rate u0) ERR_INVALID_AMOUNT)
    
    (map-set strategies
      { strategy-id: strategy-id }
      {
        name: name,
        active: true,
        total-staked: u0,
        reward-rate: reward-rate,
        last-update-block: block-height,
        accumulated-reward-per-share: u0
      }
    )
    (var-set total-strategies strategy-id)
    (ok strategy-id)
  )
)

(define-public (allocate-strategy-rewards 
  (strategy-id uint) 
  (token-contract principal) 
  (amount uint) 
  (rate-per-block uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (asserts! (is-some (get-strategy strategy-id)) ERR_STRATEGY_NOT_FOUND)
    (asserts! (is-token-supported token-contract) ERR_TOKEN_NOT_SUPPORTED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    
    (map-set strategy-token-rewards
      { strategy-id: strategy-id, token-contract: token-contract }
      {
        total-allocated: amount,
        distributed: u0,
        rate-per-block: rate-per-block
      }
    )
    (ok true)
  )
)

;; User functions
(define-public (stake-in-strategy (strategy-id uint) (amount uint))
  (let (
    (strategy (unwrap! (get-strategy strategy-id) ERR_STRATEGY_NOT_FOUND))
    (current-stake (default-to 
      { amount: u0, reward-debt: u0, last-claim-block: u0 }
      (get-user-stake tx-sender strategy-id)))
  )
    (asserts! (get active strategy) ERR_INVALID_STRATEGY)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    
    ;; Update strategy rewards before changing stakes
    (try! (update-strategy-rewards strategy-id))
    
    (let (
      (updated-strategy (unwrap! (get-strategy strategy-id) ERR_STRATEGY_NOT_FOUND))
      (new-amount (+ (get amount current-stake) amount))
      (new-debt (/ (* new-amount (get accumulated-reward-per-share updated-strategy)) u1000000))
    )
      ;; Update user stake
      (map-set user-stakes
        { user: tx-sender, strategy-id: strategy-id }
        {
          amount: new-amount,
          reward-debt: new-debt,
          last-claim-block: block-height
        }
      )
      
      ;; Update strategy total
      (map-set strategies
        { strategy-id: strategy-id }
        (merge updated-strategy {
          total-staked: (+ (get total-staked updated-strategy) amount)
        })
      )
      
      (ok true)
    )
  )
)

(define-public (unstake-from-strategy (strategy-id uint) (amount uint))
  (let (
    (strategy (unwrap! (get-strategy strategy-id) ERR_STRATEGY_NOT_FOUND))
    (current-stake (unwrap! (get-user-stake tx-sender strategy-id) ERR_INSUFFICIENT_BALANCE))
  )
    (asserts! (>= (get amount current-stake) amount) ERR_INSUFFICIENT_BALANCE)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    
    ;; Update strategy rewards before changing stakes
    (try! (update-strategy-rewards strategy-id))
    
    (let (
      (updated-strategy (unwrap! (get-strategy strategy-id) ERR_STRATEGY_NOT_FOUND))
      (new-amount (- (get amount current-stake) amount))
      (new-debt (/ (* new-amount (get accumulated-reward-per-share updated-strategy)) u1000000))
    )
      ;; Update user stake
      (map-set user-stakes
        { user: tx-sender, strategy-id: strategy-id }
        {
          amount: new-amount,
          reward-debt: new-debt,
          last-claim-block: block-height
        }
      )
      
      ;; Update strategy total
      (map-set strategies
        { strategy-id: strategy-id }
        (merge updated-strategy {
          total-staked: (- (get total-staked updated-strategy) amount)
        })
      )
      
      (ok true)
    )
  )
)

(define-public (claim-rewards (strategy-id uint) (token-contract principal))
  (let (
    (strategy (unwrap! (get-strategy strategy-id) ERR_STRATEGY_NOT_FOUND))
    (user-stake (unwrap! (get-user-stake tx-sender strategy-id) ERR_NO_REWARDS))
    (current-epoch (var-get reward-epoch))
  )
    (asserts! (is-token-supported token-contract) ERR_TOKEN_NOT_SUPPORTED)
    
    ;; Update strategy rewards
    (try! (update-strategy-rewards strategy-id))
    
    (let (
      (updated-strategy (unwrap! (get-strategy strategy-id) ERR_STRATEGY_NOT_FOUND))
      ;; Ensure variable name doesn't conflict with user-rewards map
      (pending-reward-amount (/ (- (* (get amount user-stake) (get accumulated-reward-per-share updated-strategy))
                                  (* (get reward-debt user-stake) u1000000)) u1000000))
    )
      (asserts! (> pending-reward-amount u0) ERR_NO_REWARDS)
      
      ;; Update user reward debt
      (map-set user-stakes
        { user: tx-sender, strategy-id: strategy-id }
        (merge user-stake {
          reward-debt: (/ (* (get amount user-stake) (get accumulated-reward-per-share updated-strategy)) u1000000),
          last-claim-block: block-height
        })
      )
      
      ;; Record user rewards for this epoch
      (map-set user-rewards
        { user: tx-sender, token-contract: token-contract, epoch: current-epoch }
        {
          amount: pending-reward-amount,
          claimed: true
        }
      )
      
      ;; Update token distribution stats
      (match (map-get? reward-tokens { token-contract: token-contract })
        token-info (map-set reward-tokens
          { token-contract: token-contract }
          (merge token-info {
            total-distributed: (+ (get total-distributed token-info) pending-reward-amount)
          }))
        false
      )
      
      (ok pending-reward-amount)
    )
  )
)

(define-public (distribute-epoch-rewards)
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (var-set reward-epoch (+ (var-get reward-epoch) u1))
    (ok (var-get reward-epoch))
  )
)

;; Emergency functions
(define-public (pause-strategy (strategy-id uint))
  (let (
    (strategy (unwrap! (get-strategy strategy-id) ERR_STRATEGY_NOT_FOUND))
  )
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (map-set strategies
      { strategy-id: strategy-id }
      (merge strategy { active: false })
    )
    (ok true)
  )
)

(define-public (resume-strategy (strategy-id uint))
  (let (
    (strategy (unwrap! (get-strategy strategy-id) ERR_STRATEGY_NOT_FOUND))
  )
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (map-set strategies
      { strategy-id: strategy-id }
      (merge strategy { active: true })
    )
    (ok true)
  )
)
