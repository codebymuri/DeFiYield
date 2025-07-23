;; Automated Rebalancing Manager Smart Contract
;; Handles AI-driven asset reallocation between yield pools

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-OWNER-ONLY (err u200))
(define-constant ERR-NOT-AUTHORIZED (err u201))
(define-constant ERR-INVALID-POOL (err u202))
(define-constant ERR-INSUFFICIENT-BALANCE (err u203))
(define-constant ERR-REBALANCING-PAUSED (err u204))
(define-constant ERR-INVALID-ALLOCATION (err u205))
(define-constant ERR-SLIPPAGE-EXCEEDED (err u206))
(define-constant ERR-COOLDOWN-ACTIVE (err u207))
(define-constant ERR-INVALID-ORACLE (err u208))
(define-constant ERR-STALE-DATA (err u209))
(define-constant ERR-TRANSFER-FAILED (err u210))

;; Rebalancing strategy types
(define-constant STRATEGY-CONSERVATIVE u1)
(define-constant STRATEGY-MODERATE u2)
(define-constant STRATEGY-AGGRESSIVE u3)
(define-constant STRATEGY-AI-DRIVEN u4)

;; Data Variables
(define-data-var rebalancing-paused bool false)
(define-data-var current-strategy uint STRATEGY-MODERATE)
(define-data-var rebalancing-cooldown uint u144) ;; blocks (~24 hours)
(define-data-var last-rebalance-block uint u0)
(define-data-var max-slippage uint u500) ;; 5% in basis points
(define-data-var total-managed-assets uint u0)
(define-data-var rebalancing-fee uint u50) ;; 0.5% in basis points

;; Data Maps
(define-map authorized-oracles principal bool)
(define-map authorized-agents principal bool)
(define-map yield-pools uint {
  contract: principal,
  target-allocation: uint, ;; basis points (10000 = 100%)
  current-allocation: uint,
  min-allocation: uint,
  max-allocation: uint,
  last-yield: uint,
  is-active: bool
})
(define-map pool-balances uint uint)
(define-map oracle-data principal {
  last-update: uint,
  data-hash: (buff 32),
  confidence: uint,
  is-valid: bool
})
(define-map rebalancing-history uint {
  stacks-block-height: uint,
  strategy-used: uint,
  pools-affected: (list 10 uint),
  total-moved: uint,
  gas-used: uint
})

;; Pool allocation recommendations from AI/Oracle
(define-map ai-recommendations uint {
  pool-id: uint,
  recommended-allocation: uint,
  confidence-score: uint,
  timestamp: uint,
  reasoning: (string-ascii 256)
})

;; SIP-010 Token Trait
(define-trait sip-010-trait
  (
    (transfer (uint principal principal (optional (buff 34))) (response bool uint))
    (get-balance (principal) (response uint uint))
  )
)

;; Pool Interface Trait
(define-trait pool-trait
  (
    (deposit (uint) (response uint uint))
    (withdraw (uint) (response uint uint))
    (get-pool-balance () (response uint uint))
    (get-user-balance (principal) (response uint uint))
  )
)

;; Read-only functions

(define-read-only (get-rebalancing-info)
  {
    is-paused: (var-get rebalancing-paused),
    current-strategy: (var-get current-strategy),
    cooldown-blocks: (var-get rebalancing-cooldown),
    last-rebalance: (var-get last-rebalance-block),
    max-slippage: (var-get max-slippage),
    total-assets: (var-get total-managed-assets),
    rebalancing-fee: (var-get rebalancing-fee)
  }
)

(define-read-only (get-pool-info (pool-id uint))
  (map-get? yield-pools pool-id)
)

(define-read-only (get-pool-balance (pool-id uint))
  (default-to u0 (map-get? pool-balances pool-id))
)

(define-read-only (is-rebalancing-needed)
  (let (
    (last-rebalance (var-get last-rebalance-block))
    (cooldown (var-get rebalancing-cooldown))
    (current-block stacks-block-height)
  )
    (and 
      (not (var-get rebalancing-paused))
      (>= (- current-block last-rebalance) cooldown)
      (> (calculate-allocation-drift) u200) ;; 2% drift threshold
    )
  )
)

(define-read-only (calculate-allocation-drift)
  (fold calculate-pool-drift (list u1 u2 u3 u4 u5) u0)
)

(define-read-only (get-ai-recommendation (pool-id uint))
  (map-get? ai-recommendations pool-id)
)

(define-read-only (is-authorized-oracle (oracle principal))
  (default-to false (map-get? authorized-oracles oracle))
)

(define-read-only (is-authorized-agent (agent principal))
  (default-to false (map-get? authorized-agents agent))
)

;; Private functions

(define-private (calculate-pool-drift (pool-id uint) (total-drift uint))
  (match (map-get? yield-pools pool-id)
    pool-data (let (
      (target (get target-allocation pool-data))
      (current (get current-allocation pool-data))
      (drift (if (> target current) (- target current) (- current target)))
    )
      (+ total-drift drift)
    )
    total-drift
  )
)

(define-private (validate-allocation-sum (allocations (list 10 {pool-id: uint, allocation: uint})))
  (let (
    (total-allocation (fold sum-allocations allocations u0))
  )
    (<= total-allocation u10000) ;; Must not exceed 100%
  )
)

(define-private (sum-allocations (item {pool-id: uint, allocation: uint}) (sum uint))
  (+ sum (get allocation item))
)

(define-private (execute-pool-rebalance (pool-id uint) (target-amount uint))
  (match (map-get? yield-pools pool-id)
    pool-data (let (
      (current-balance (default-to u0 (map-get? pool-balances pool-id)))
      (pool-contract (get contract pool-data))
    )
      (if (> target-amount current-balance)
        ;; Need to deposit more
        (deposit-to-pool pool-id (- target-amount current-balance))
        ;; Need to withdraw some
        (withdraw-from-pool pool-id (- current-balance target-amount))
      )
    )
    ERR-INVALID-POOL
  )
)

(define-private (deposit-to-pool (pool-id uint) (amount uint))
  (match (map-get? yield-pools pool-id)
    pool-data (let (
      (pool-contract (get contract pool-data))
      (current-balance (default-to u0 (map-get? pool-balances pool-id)))
    )
      ;; In a real implementation, you'd call the actual pool contract
      (map-set pool-balances pool-id (+ current-balance amount))
      (ok amount)
    )
    ERR-INVALID-POOL
  )
)

(define-private (withdraw-from-pool (pool-id uint) (amount uint))
  (match (map-get? yield-pools pool-id)
    pool-data (let (
      (pool-contract (get contract pool-data))
      (current-balance (default-to u0 (map-get? pool-balances pool-id)))
    )
      (asserts! (>= current-balance amount) ERR-INSUFFICIENT-BALANCE)
      ;; In a real implementation, you'd call the actual pool contract
      (map-set pool-balances pool-id (- current-balance amount))
      (ok amount)
    )
    ERR-INVALID-POOL
  )
)

(define-private (calculate-rebalancing-amounts (strategy uint))
  (if (is-eq strategy STRATEGY-AI-DRIVEN)
    (calculate-ai-driven-allocation)
    (calculate-traditional-allocation strategy)
  )
)

(define-private (calculate-ai-driven-allocation)
  ;; Use AI recommendations with confidence weighting
  (let (
    (total-assets (var-get total-managed-assets))
  )
    ;; This would integrate with oracle data in a real implementation
    (list 
      {pool-id: u1, target-amount: (/ (* total-assets u3000) u10000)}
      {pool-id: u2, target-amount: (/ (* total-assets u4000) u10000)}
      {pool-id: u3, target-amount: (/ (* total-assets u3000) u10000)}
    )
  )
)

(define-private (calculate-traditional-allocation (strategy uint))
  (let (
    (total-assets (var-get total-managed-assets))
  )
    (if (is-eq strategy STRATEGY-CONSERVATIVE)
      ;; Conservative: 60% stable, 30% moderate risk, 10% high risk
      (list 
        {pool-id: u1, target-amount: (/ (* total-assets u6000) u10000)}
        {pool-id: u2, target-amount: (/ (* total-assets u3000) u10000)}
        {pool-id: u3, target-amount: (/ (* total-assets u1000) u10000)}
      )
      ;; Moderate or Aggressive strategies
      (list 
        {pool-id: u1, target-amount: (/ (* total-assets u4000) u10000)}
        {pool-id: u2, target-amount: (/ (* total-assets u4000) u10000)}
        {pool-id: u3, target-amount: (/ (* total-assets u2000) u10000)}
      )
    )
  )
)

;; Public functions

(define-public (register-yield-pool (pool-id uint) (contract-address principal) (target-allocation uint) (min-allocation uint) (max-allocation uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (asserts! (<= target-allocation u10000) ERR-INVALID-ALLOCATION)
    (asserts! (<= min-allocation max-allocation) ERR-INVALID-ALLOCATION)
    
    (map-set yield-pools pool-id {
      contract: contract-address,
      target-allocation: target-allocation,
      current-allocation: u0,
      min-allocation: min-allocation,
      max-allocation: max-allocation,
      last-yield: u0,
      is-active: true
    })
    
    (print {
      event: "pool-registered",
      pool-id: pool-id,
      contract: contract-address,
      target-allocation: target-allocation
    })
    
    (ok true)
  )
)

(define-public (execute-rebalancing)
  (let (
    (sender tx-sender)
    (current-block stacks-block-height)
    (last-rebalance (var-get last-rebalance-block))
    (cooldown (var-get rebalancing-cooldown))
    (strategy-type (var-get current-strategy))
  )
    ;; Authorization checks
    (asserts! (or 
      (is-eq sender CONTRACT-OWNER)
      (default-to false (map-get? authorized-agents sender))
    ) ERR-NOT-AUTHORIZED)
    
    ;; Validation checks
    (asserts! (not (var-get rebalancing-paused)) ERR-REBALANCING-PAUSED)
    (asserts! (>= (- current-block last-rebalance) cooldown) ERR-COOLDOWN-ACTIVE)
    
    ;; Calculate new allocations
    (let (
      (target-allocations (calculate-rebalancing-amounts strategy-type))
      (rebalance-id (+ (var-get last-rebalance-block) u1))
    )
      ;; Execute rebalancing for each pool
      (match (fold execute-rebalancing-step target-allocations (ok u0))
        success (begin
          ;; Update state
          (var-set last-rebalance-block current-block)
          
          ;; Record rebalancing history
          (map-set rebalancing-history rebalance-id {
            stacks-block-height: current-block,
            strategy-used: strategy-type,
            pools-affected: (list u1 u2 u3),
            total-moved: success,
            gas-used: u0 ;; Would be calculated in real implementation
          })
          
          (print {
            event: "rebalancing-executed",
            strategy: strategy-type,
            total-moved: success,
            stacks-block-height: current-block
          })
          
          (ok success)
        )
        error (err error)
      )
    )
  )
)

(define-private (execute-rebalancing-step (allocation {pool-id: uint, target-amount: uint}) (result (response uint uint)))
  (match result
    success (match (execute-pool-rebalance (get pool-id allocation) (get target-amount allocation))
      pool-result (ok (+ success pool-result))
      pool-error (err pool-error)
    )
    error (err error)
  )
)

(define-public (submit-ai-recommendation (pool-id uint) (recommended-allocation uint) (confidence-score uint) (reasoning (string-ascii 256)))
  (let (
    (sender tx-sender)
  )
    (asserts! (default-to false (map-get? authorized-oracles sender)) ERR-NOT-AUTHORIZED)
    (asserts! (<= recommended-allocation u10000) ERR-INVALID-ALLOCATION)
    (asserts! (<= confidence-score u100) ERR-INVALID-ALLOCATION)
    
    (map-set ai-recommendations pool-id {
      pool-id: pool-id,
      recommended-allocation: recommended-allocation,
      confidence-score: confidence-score,
      timestamp: stacks-block-height,
      reasoning: reasoning
    })
    
    (print {
      event: "ai-recommendation-submitted",
      pool-id: pool-id,
      allocation: recommended-allocation,
      confidence: confidence-score,
      oracle: sender
    })
    
    (ok true)
  )
)

(define-public (trigger-emergency-rebalancing (target-allocations (list 10 {pool-id: uint, allocation: uint})))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (asserts! (validate-allocation-sum target-allocations) ERR-INVALID-ALLOCATION)
    
    ;; Force rebalancing regardless of cooldown
    (var-set last-rebalance-block (- stacks-block-height (var-get rebalancing-cooldown)))
    
    (print { event: "emergency-rebalancing-triggered" })
    (execute-rebalancing)
  )
)

;; Oracle and Agent Management

(define-public (authorize-oracle (oracle principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (map-set authorized-oracles oracle true)
    (print { event: "oracle-authorized", oracle: oracle })
    (ok true)
  )
)

(define-public (revoke-oracle (oracle principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (map-set authorized-oracles oracle false)
    (print { event: "oracle-revoked", oracle: oracle })
    (ok true)
  )
)

(define-public (authorize-agent (agent principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (map-set authorized-agents agent true)
    (print { event: "agent-authorized", agent: agent })
    (ok true)
  )
)

(define-public (revoke-agent (agent principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (map-set authorized-agents agent false)
    (print { event: "agent-revoked", agent: agent })
    (ok true)
  )
)

;; Configuration Management

(define-public (set-rebalancing-strategy (strategy uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (asserts! (and (>= strategy u1) (<= strategy u4)) ERR-INVALID-ALLOCATION)
    
    (var-set current-strategy strategy)
    (print { event: "strategy-updated", new-strategy: strategy })
    (ok true)
  )
)

(define-public (set-rebalancing-cooldown (blocks uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (var-set rebalancing-cooldown blocks)
    (print { event: "cooldown-updated", new-cooldown: blocks })
    (ok true)
  )
)

(define-public (set-max-slippage (slippage uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (asserts! (<= slippage u1000) ERR-INVALID-ALLOCATION) ;; Max 10%
    
    (var-set max-slippage slippage)
    (print { event: "slippage-updated", new-slippage: slippage })
    (ok true)
  )
)

(define-public (pause-rebalancing)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (var-set rebalancing-paused true)
    (print { event: "rebalancing-paused" })
    (ok true)
  )
)

(define-public (unpause-rebalancing)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (var-set rebalancing-paused false)
    (print { event: "rebalancing-unpaused" })
    (ok true)
  )
)

;; Asset Management

(define-public (update-total-assets (new-total uint))
  (begin
    (asserts! (or 
      (is-eq tx-sender CONTRACT-OWNER)
      (default-to false (map-get? authorized-agents tx-sender))
    ) ERR-NOT-AUTHORIZED)
    
    (var-set total-managed-assets new-total)
    (print { event: "total-assets-updated", new-total: new-total })
    (ok true)
  )
)

(define-public (update-pool-balance (pool-id uint) (new-balance uint))
  (begin
    (asserts! (or 
      (is-eq tx-sender CONTRACT-OWNER)
      (default-to false (map-get? authorized-agents tx-sender))
    ) ERR-NOT-AUTHORIZED)
    
    (map-set pool-balances pool-id new-balance)
    (print { event: "pool-balance-updated", pool-id: pool-id, balance: new-balance })
    (ok true)
  )
)

;; Scheduled rebalancing trigger (called by off-chain agents)
(define-public (scheduled-rebalancing-check)
  (let (
    (sender tx-sender)
  )
    (asserts! (default-to false (map-get? authorized-agents sender)) ERR-NOT-AUTHORIZED)
    
    (if (is-rebalancing-needed)
      (execute-rebalancing)
      (ok u0)
    )
  )
)