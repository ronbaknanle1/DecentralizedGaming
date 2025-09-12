;; Asset Leasing Pool System - Collective gaming asset investment

;; Error constants
(define-constant ERR-UNAUTHORIZED (err u800))
(define-constant ERR-NOT-FOUND (err u801))
(define-constant ERR-INVALID-AMOUNT (err u802))
(define-constant ERR-POOL-CLOSED (err u803))
(define-constant ERR-INSUFFICIENT-FUNDS (err u804))

;; Pool status and fee constants
(define-constant POOL-FUNDRAISING "fundraising")
(define-constant POOL-ACTIVE "active")
(define-constant POOL-LIQUIDATED "liquidated")
(define-constant MANAGEMENT-FEE-RATE u200)

;; Asset investment pools
(define-map investment-pools
  { pool-id: uint }
  {
    creator: principal,
    target-asset-id: uint,
    funding-goal: uint,
    total-raised: uint,
    asset-current-value: uint,
    total-rental-income: uint,
    status: (string-ascii 20),
    created-at: uint,
    participant-count: uint
  }
)

;; Individual contributions to pools
(define-map pool-contributions
  { pool-id: uint, contributor: principal }
  {
    amount-invested: uint,
    share-percentage: uint,
    total-claimed: uint
  }
)

;; Global counters
(define-data-var pool-counter uint u0)

;; Create new asset investment pool
(define-public (create-pool (target-asset-id uint) (funding-goal uint))
  (let (
    (pool-id (+ (var-get pool-counter) u1))
  )
    (asserts! (> funding-goal u0) ERR-INVALID-AMOUNT)
    
    (var-set pool-counter pool-id)
    (map-set investment-pools
      { pool-id: pool-id }
      {
        creator: tx-sender,
        target-asset-id: target-asset-id,
        funding-goal: funding-goal,
        total-raised: u0,
        asset-current-value: u0,
        total-rental-income: u0,
        status: POOL-FUNDRAISING,
        created-at: stacks-block-height,
        participant-count: u0
      }
    )
    (ok pool-id)
  )
)

;; Contribute STX to investment pool
(define-public (contribute-to-pool (pool-id uint) (amount uint))
  (let (
    (pool (unwrap! (map-get? investment-pools { pool-id: pool-id }) ERR-NOT-FOUND))
    (existing-contribution (default-to 
      { amount-invested: u0, share-percentage: u0, total-claimed: u0 }
      (map-get? pool-contributions { pool-id: pool-id, contributor: tx-sender })
    ))
  )
    (asserts! (is-eq (get status pool) POOL-FUNDRAISING) ERR-POOL-CLOSED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (<= (+ (get total-raised pool) amount) (get funding-goal pool)) ERR-INVALID-AMOUNT)
    
    ;; Transfer contribution to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Update pool totals
    (let (
      (new-total-raised (+ (get total-raised pool) amount))
      (new-participant-count (if (is-eq (get amount-invested existing-contribution) u0)
        (+ (get participant-count pool) u1)
        (get participant-count pool)
      ))
    )
      (map-set investment-pools
        { pool-id: pool-id }
        (merge pool {
          total-raised: new-total-raised,
          participant-count: new-participant-count
        })
      )
      
      ;; Update contributor record
      (map-set pool-contributions
        { pool-id: pool-id, contributor: tx-sender }
        {
          amount-invested: (+ (get amount-invested existing-contribution) amount),
          share-percentage: (/ (* (+ (get amount-invested existing-contribution) amount) u10000) (get funding-goal pool)),
          total-claimed: (get total-claimed existing-contribution)
        }
      )
      
      ;; Auto-activate pool if funding goal reached
      (if (is-eq new-total-raised (get funding-goal pool))
        (try! (activate-pool pool-id))
        true
      )
    )
    (ok true)
  )
)

;; Activate pool once funding goal is reached (internal)
(define-private (activate-pool (pool-id uint))
  (let (
    (pool (unwrap! (map-get? investment-pools { pool-id: pool-id }) ERR-NOT-FOUND))
  )
    (asserts! (is-eq (get status pool) POOL-FUNDRAISING) ERR-POOL-CLOSED)
    (asserts! (is-eq (get total-raised pool) (get funding-goal pool)) ERR-INSUFFICIENT-FUNDS)
    
    (map-set investment-pools
      { pool-id: pool-id }
      (merge pool {
        status: POOL-ACTIVE,
        asset-current-value: (get funding-goal pool)
      })
    )
    (ok true)
  )
)

;; Record rental income for pool
(define-public (record-pool-rental (pool-id uint) (renter principal) (rental-amount uint))
  (let (
    (pool (unwrap! (map-get? investment-pools { pool-id: pool-id }) ERR-NOT-FOUND))
  )
    (asserts! (is-eq (get status pool) POOL-ACTIVE) ERR-POOL-CLOSED)
    (asserts! (> rental-amount u0) ERR-INVALID-AMOUNT)
    
    ;; Transfer rental payment to contract
    (try! (stx-transfer? rental-amount renter (as-contract tx-sender)))
    
    ;; Calculate management fee
    (let (
      (management-fee (/ (* rental-amount MANAGEMENT-FEE-RATE) u10000))
      (net-rental-income (- rental-amount management-fee))
    )
      ;; Pay management fee to pool creator
      (try! (as-contract (stx-transfer? management-fee tx-sender (get creator pool))))
      
      ;; Update pool income
      (map-set investment-pools
        { pool-id: pool-id }
        (merge pool { total-rental-income: (+ (get total-rental-income pool) net-rental-income) })
      )
    )
    (ok true)
  )
)

;; Claim proportional rental income
(define-public (claim-rental-income (pool-id uint))
  (let (
    (pool (unwrap! (map-get? investment-pools { pool-id: pool-id }) ERR-NOT-FOUND))
    (contribution (unwrap! (map-get? pool-contributions { pool-id: pool-id, contributor: tx-sender }) ERR-NOT-FOUND))
  )
    (asserts! (is-eq (get status pool) POOL-ACTIVE) ERR-POOL-CLOSED)
    (asserts! (> (get amount-invested contribution) u0) ERR-UNAUTHORIZED)
    
    ;; Calculate claimable amount based on share percentage
    (let (
      (total-claimable (/ (* (get total-rental-income pool) (get share-percentage contribution)) u10000))
      (unclaimed-amount (- total-claimable (get total-claimed contribution)))
    )
      (asserts! (> unclaimed-amount u0) ERR-INSUFFICIENT-FUNDS)
      
      ;; Transfer claimable income to contributor
      (try! (as-contract (stx-transfer? unclaimed-amount tx-sender tx-sender)))
      
      ;; Update claim record
      (map-set pool-contributions
        { pool-id: pool-id, contributor: tx-sender }
        (merge contribution {
          total-claimed: total-claimable
        })
      )
      (ok unclaimed-amount)
    )
  )
)


;; Read-only functions

(define-read-only (get-pool-info (pool-id uint))
  (map-get? investment-pools { pool-id: pool-id })
)

(define-read-only (get-contribution-info (pool-id uint) (contributor principal))
  (map-get? pool-contributions { pool-id: pool-id, contributor: contributor })
)

