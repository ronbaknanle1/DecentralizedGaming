;; DecentralizedGaming - Gaming Asset Marketplace
;; Core features: Asset listing, trading, premium subscriptions, tournament access

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-listed (err u102))
(define-constant err-insufficient-balance (err u103))
(define-constant err-not-authorized (err u104))
(define-constant err-invalid-price (err u105))


(define-map asset-price-history
    { asset-id: uint }
    {
        base-price: uint,
        current-multiplier: uint,
        last-sale-block: uint,
        total-views: uint,
        failed-purchases: uint,
        successful-sales: uint
    }
)

(define-map market-demand-metrics
    { asset-id: uint }
    {
        view-count-24h: uint,
        purchase-attempts-24h: uint,
        last-metric-reset: uint,
        demand-score: uint
    }
)

(define-map price-adjustment-rules
    { rule-id: uint }
    {
        min-multiplier: uint,
        max-multiplier: uint,
        adjustment-rate: uint,
        cooldown-period: uint
    }
)

(define-data-var price-engine-enabled bool true)
(define-data-var base-multiplier uint u1000)
(define-data-var max-price-increase uint u2000)
(define-data-var min-price-decrease uint u500)
(define-data-var demand-threshold-high uint u10)
(define-data-var demand-threshold-low uint u2)
(define-data-var price-adjustment-cooldown uint u144)


;; Data Variables
(define-data-var platform-fee uint u25) ;; 2.5% fee
(define-data-var tournament-entry-fee uint u100)
(define-data-var premium-subscription-price uint u1000)

;; Data Maps
(define-map gaming-assets 
    { asset-id: uint } 
    { 
        owner: principal,
        name: (string-ascii 50),
        price: uint,
        listed: bool,
        verified: bool
    }
)

(define-map asset-trading-history
    { asset-id: uint }
    { 
        previous-owners: (list 10 principal),
        sale-prices: (list 10 uint)
    }
)

(define-map premium-subscribers
    { user: principal }
    { 
        subscription-start: uint,
        subscription-end: uint,
        active: bool
    }
)

(define-map tournament-participants 
    { tournament-id: uint }
    { 
        participants: (list 100 principal),
        registered: uint
    }
)

;; Public Functions

;; List a gaming asset for sale
(define-public (list-asset (asset-id uint) (asset-name (string-ascii 50)) (price uint))
    (let ((asset-owner tx-sender))
        (asserts! (> price u0) err-invalid-price)
        (asserts! (not (default-to false (get listed (map-get? gaming-assets {asset-id: asset-id})))) err-already-listed)
        (ok (map-set gaming-assets 
            {asset-id: asset-id}
            {
                owner: asset-owner,
                name: asset-name,
                price: price,
                listed: true,
                verified: false
            }
        ))
    )
)

;; Purchase a listed asset
(define-public (purchase-asset (asset-id uint))
    (let (
        (asset (unwrap! (map-get? gaming-assets {asset-id: asset-id}) err-not-found))
        (buyer tx-sender)
        (price (get price asset))
        (seller (get owner asset))
        )
        (asserts! (is-eq (get listed asset) true) err-not-found)
        (asserts! (not (is-eq buyer seller)) err-not-authorized)
        
        ;; Transfer payment and update ownership
        (try! (stx-transfer? price buyer seller))
        (try! (pay-platform-fee price))
        
        ;; Update trading history
        (map-set asset-trading-history 
            {asset-id: asset-id}
            (try! (merge-history asset-id seller price))
        )
        
        ;; Update asset ownership
        (ok (map-set gaming-assets
            {asset-id: asset-id}
            (merge {owner: buyer, listed: false} asset)
        ))
    )
)

;; Subscribe for premium features
(define-public (subscribe-premium)
    (let (
        (subscriber tx-sender)
        (current-stacks-block-height stacks-block-height)
        (subscription-duration u43200) ;; Approximately 30 days in blocks
        )
        (try! (stx-transfer? (var-get premium-subscription-price) subscriber contract-owner))
        (ok (map-set premium-subscribers
            {user: subscriber}
            {
                subscription-start: current-stacks-block-height,
                subscription-end: (+ current-stacks-block-height subscription-duration),
                active: true
            }
        ))
    )
)

;; Register for tournament
(define-public (register-tournament (tournament-id uint))
    (let (
        (participant tx-sender)
        (tournament (default-to {participants: (list), registered: u0} 
            (map-get? tournament-participants {tournament-id: tournament-id})))
        )
        (try! (stx-transfer? (var-get tournament-entry-fee) participant contract-owner))
        (ok (map-set tournament-participants
            {tournament-id: tournament-id}
            {
                participants: (unwrap! (as-max-len? (append (get participants tournament) participant) u100) err-not-authorized),
                registered: (+ (get registered tournament) u1)
            }
        ))
    )
)

;; Read Only Functions

(define-read-only (get-asset (asset-id uint))
    (map-get? gaming-assets {asset-id: asset-id})
)

(define-read-only (get-asset-history (asset-id uint))
    (map-get? asset-trading-history {asset-id: asset-id})
)

(define-read-only (check-premium-status (user principal))
    (let ((subscription (map-get? premium-subscribers {user: user})))
        (match subscription
            sub (is-eq (get active sub) true)
            false
        )
    )
)

;; Private Functions

(define-private (pay-platform-fee (price uint))
    (let ((fee (/ (* price (var-get platform-fee)) u1000)))
        (stx-transfer? fee tx-sender contract-owner)
    )
)

(define-private (merge-history (asset-id uint) (seller principal) (price uint))
    (let ((current-history (default-to {previous-owners: (list), sale-prices: (list)} 
            (map-get? asset-trading-history {asset-id: asset-id}))))
        (ok {
            previous-owners: (unwrap! (as-max-len? (append (get previous-owners current-history) seller) u10) err-not-authorized),
            sale-prices: (unwrap! (as-max-len? (append (get sale-prices current-history) price) u10) err-not-authorized)
        })
    )
)


(define-map asset-rentals 
    { asset-id: uint }
    {
        renter: principal,
        rental-start: uint,
        rental-end: uint,
        rental-price: uint,
        is-rented: bool
    }
)

(define-data-var min-rental-period uint u1440)
(define-data-var max-rental-period uint u43200)

(define-public (list-asset-for-rent (asset-id uint) (rental-price uint) (duration uint))
    (let ((asset (unwrap! (map-get? gaming-assets {asset-id: asset-id}) err-not-found)))
        (asserts! (is-eq (get owner asset) tx-sender) err-not-authorized)
        (asserts! (>= duration (var-get min-rental-period)) err-invalid-price)
        (asserts! (<= duration (var-get max-rental-period)) err-invalid-price)
        (ok (map-set asset-rentals
            {asset-id: asset-id}
            {
                renter: tx-sender,
                rental-start: u0,
                rental-end: u0,
                rental-price: rental-price,
                is-rented: false
            }
        ))
    )
)

(define-public (rent-asset (asset-id uint))
    (let (
        (rental (unwrap! (map-get? asset-rentals {asset-id: asset-id}) err-not-found))
        (current-height stacks-block-height)
        )
        (asserts! (not (get is-rented rental)) err-already-listed)
        (try! (stx-transfer? (get rental-price rental) tx-sender (get owner (unwrap! (map-get? gaming-assets {asset-id: asset-id}) err-not-found))))
        (ok (map-set asset-rentals
            {asset-id: asset-id}
            (merge rental {
                renter: tx-sender,
                rental-start: current-height,
                rental-end: (+ current-height (var-get min-rental-period)),
                is-rented: true
            })
        ))
    )
)


(define-map asset-bundles 
    { bundle-id: uint }
    {
        owner: principal,
        asset-ids: (list 5 uint),
        bundle-price: uint,
        active: bool
    }
)

(define-data-var bundle-counter uint u0)

(define-public (create-bundle (asset-ids (list 5 uint)) (bundle-price uint))
    (let (
        (bundle-id (+ (var-get bundle-counter) u1))
        )
        (var-set bundle-counter bundle-id)
        (ok (map-set asset-bundles
            {bundle-id: bundle-id}
            {
                owner: tx-sender,
                asset-ids: asset-ids,
                bundle-price: bundle-price,
                active: true
            }
        ))
    )
)

(define-public (purchase-bundle (bundle-id uint))
    (let (
        (bundle (unwrap! (map-get? asset-bundles {bundle-id: bundle-id}) err-not-found))
        )
        (asserts! (get active bundle) err-not-found)
        (try! (stx-transfer? (get bundle-price bundle) tx-sender (get owner bundle)))
        (ok (map-set asset-bundles
            {bundle-id: bundle-id}
            (merge bundle {active: false})
        ))
    )
)


(define-map asset-auctions
    { asset-id: uint }
    {
        seller: principal,
        current-bid: uint,
        highest-bidder: (optional principal),
        end-block: uint,
        active: bool
    }
)

(define-data-var min-auction-duration uint u1440)
(define-constant min-bid-increase  u100)

(define-public (start-auction (asset-id uint) (start-price uint) (duration uint))
    (let (
        (asset (unwrap! (map-get? gaming-assets {asset-id: asset-id}) err-not-found))
        )
        (asserts! (is-eq (get owner asset) tx-sender) err-not-authorized)
        (asserts! (>= duration (var-get min-auction-duration)) err-invalid-price)
        (ok (map-set asset-auctions
            {asset-id: asset-id}
            {
                seller: tx-sender,
                current-bid: start-price,
                highest-bidder: none,
                end-block: (+ stacks-block-height duration),
                active: true
            }
        ))
    )
)

(define-public (place-bid (asset-id uint) (bid-amount uint))
    (let (
        (auction (unwrap! (map-get? asset-auctions {asset-id: asset-id}) err-not-found))
        )
        (asserts! (get active auction) err-not-found)
        (asserts! (<= stacks-block-height (get end-block auction)) err-not-authorized)
        (asserts! (> bid-amount (+ (get current-bid auction) min-bid-increase)) err-invalid-price)
        (try! (stx-transfer? bid-amount tx-sender (get seller auction)))
        (ok (map-set asset-auctions
            {asset-id: asset-id}
            (merge auction {
                current-bid: bid-amount,
                highest-bidder: (some tx-sender)
            })
        ))
    )
)


(define-map player-achievements
    { user: principal }
    {
        trading-volume: uint,
        assets-owned: uint,
        tournament-wins: uint,
        rank: (string-ascii 20)
    }
)

(define-map achievement-rewards
    { achievement-id: uint }
    {
        name: (string-ascii 50),
        requirement: uint,
        reward-amount: uint
    }
)

(define-public (update-achievements (user principal))
    (let (
        (current-achievements (default-to 
            {trading-volume: u0, assets-owned: u0, tournament-wins: u0, rank: "Novice"}
            (map-get? player-achievements {user: user})))
        )
        (ok (map-set player-achievements
            {user: user}
            (merge current-achievements {
                rank: (get-rank (get trading-volume current-achievements))
            })
        ))
    )
)

(define-private (get-rank (volume uint))
    (if (>= volume u1000000)
        "Diamond"
        (if (>= volume u100000)
            "Gold"
            (if (>= volume u10000)
                "Silver"
                "Bronze"
            )
        )
    )
)


(define-map referral-system
    { referrer: principal }
    {
        referred-users: (list 100 principal),
        total-rewards: uint,
        active-referrals: uint
    }
)

(define-data-var referral-reward-percentage uint u50)

(define-public (register-referral (referrer principal))
    (let (
        (current-data (default-to
            {referred-users: (list), total-rewards: u0, active-referrals: u0}
            (map-get? referral-system {referrer: referrer})))
        )
        (asserts! (not (is-eq tx-sender referrer)) err-not-authorized)
        (ok (map-set referral-system
            {referrer: referrer}
            {
                referred-users: (unwrap! (as-max-len? (append (get referred-users current-data) tx-sender) u100) err-not-authorized),
                total-rewards: (get total-rewards current-data),
                active-referrals: (+ (get active-referrals current-data) u1)
            }
        ))
    )
)

(define-public (claim-referral-rewards (referrer principal))
    (let (
        (reward-data (unwrap! (map-get? referral-system {referrer: referrer}) err-not-found))
        )
        (ok (stx-transfer? (get total-rewards reward-data) contract-owner referrer))
    )
)


(define-map verified-creators
    { creator: principal }
    {
        verification-date: uint,
        creator-name: (string-ascii 50),
        total-verified-assets: uint
    }
)

(define-map verification-requests
    { asset-id: uint }
    {
        creator: principal,
        submission-date: uint,
        status: (string-ascii 20)
    }
)

(define-public (request-asset-verification (asset-id uint))
    (let (
        (asset (unwrap! (map-get? gaming-assets {asset-id: asset-id}) err-not-found))
        )
        (asserts! (is-eq (get owner asset) tx-sender) err-not-authorized)
        (ok (map-set verification-requests
            {asset-id: asset-id}
            {
                creator: tx-sender,
                submission-date: stacks-block-height,
                status: "pending"
            }
        ))
    )
)

(define-public (verify-asset (asset-id uint))
    (let (
        (request (unwrap! (map-get? verification-requests {asset-id: asset-id}) err-not-found))
        )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set gaming-assets
            {asset-id: asset-id}
            (merge (unwrap! (map-get? gaming-assets {asset-id: asset-id}) err-not-found)
                {verified: true})
        )
        (ok (map-set verification-requests
            {asset-id: asset-id}
            (merge request {status: "verified"})
        ))
    )
)


(define-map asset-loans 
    { asset-id: uint }
    {
        owner: principal,
        borrower: (optional principal),
        collateral-amount: uint,
        loan-duration: uint,
        loan-start: uint,
        is-active: bool
    }
)

(define-data-var min-collateral-ratio uint u150)
(define-data-var max-loan-duration uint u4320)

(define-public (create-loan-offer (asset-id uint) (collateral uint) (duration uint))
    (let ((asset (unwrap! (map-get? gaming-assets {asset-id: asset-id}) err-not-found)))
        (asserts! (is-eq (get owner asset) tx-sender) err-not-authorized)
        (asserts! (<= duration (var-get max-loan-duration)) err-invalid-price)
        (ok (map-set asset-loans
            {asset-id: asset-id}
            {
                owner: tx-sender,
                borrower: none,
                collateral-amount: collateral,
                loan-duration: duration,
                loan-start: u0,
                is-active: true
            }
        ))
    )
)

(define-public (borrow-asset (asset-id uint))
    (let (
        (loan (unwrap! (map-get? asset-loans {asset-id: asset-id}) err-not-found))
        (current-height stacks-block-height)
        )
        (asserts! (get is-active loan) err-not-found)
        (asserts! (is-none (get borrower loan)) err-already-listed)
        (try! (stx-transfer? (get collateral-amount loan) tx-sender (get owner loan)))
        (ok (map-set asset-loans
            {asset-id: asset-id}
            (merge loan {
                borrower: (some tx-sender),
                loan-start: current-height
            })
        ))
    )
)



(define-public (repay-loan (asset-id uint))
    (let (
        (loan (unwrap! (map-get? asset-loans {asset-id: asset-id}) err-not-found))
        )
        (asserts! (is-some (get borrower loan)) err-not-authorized)
        (asserts! (< stacks-block-height (+ (get loan-start loan) (get loan-duration loan))) err-not-authorized)
        (try! (stx-transfer? (get collateral-amount loan) tx-sender (get owner loan)))
        (ok (map-set asset-loans
            {asset-id: asset-id}
            {
                owner: tx-sender,
                borrower: none,
                collateral-amount: u0,
                loan-duration: u0,
                loan-start: u0,
                is-active: false
            }
        ))
    )
)

(define-public (unstake-asset (asset-id uint))
    (let (
        (stake-info (unwrap! (map-get? staked-assets {asset-id: asset-id}) err-not-found))
        )
        (asserts! (get is-staked stake-info) err-not-found)
        (asserts! (is-eq (get staker stake-info) tx-sender) err-not-authorized)
        (ok (map-set staked-assets
            {asset-id: asset-id}
            {
                staker: tx-sender,
                stake-start: u0,
                stake-duration: u0,
                rewards-claimed: u0,
                is-staked: false
            }
        ))
    )
)

(define-public (cancel-loan-offer (asset-id uint))
    (let (
        (loan (unwrap! (map-get? asset-loans {asset-id: asset-id}) err-not-found))
        )
        (asserts! (is-eq tx-sender (get owner loan)) err-not-authorized)
        (ok (map-set asset-loans
            {asset-id: asset-id}
            {
                owner: tx-sender,
                borrower: none,
                collateral-amount: u0,
                loan-duration: u0,
                loan-start: u0,
                is-active: false
            }
        ))
    )
)


(define-map staked-assets
    { asset-id: uint }
    {
        staker: principal,
        stake-start: uint,
        stake-duration: uint,
        rewards-claimed: uint,
        is-staked: bool
    }
)

(define-data-var reward-rate uint u100)
(define-data-var min-stake-duration uint u1440)

(define-public (stake-asset (asset-id uint) (duration uint))
    (let ((asset (unwrap! (map-get? gaming-assets {asset-id: asset-id}) err-not-found)))
        (asserts! (is-eq (get owner asset) tx-sender) err-not-authorized)
        (asserts! (>= duration (var-get min-stake-duration)) err-invalid-price)
        (ok (map-set staked-assets
            {asset-id: asset-id}
            {
                staker: tx-sender,
                stake-start: stacks-block-height,
                stake-duration: duration,
                rewards-claimed: u0,
                is-staked: true
            }
        ))
    )
)

(define-public (claim-staking-rewards (asset-id uint))
    (let (
        (stake-info (unwrap! (map-get? staked-assets {asset-id: asset-id}) err-not-found))
        (elapsed-blocks (- stacks-block-height (get stake-start stake-info)))
        (reward-amount (* elapsed-blocks (var-get reward-rate)))
        )
        (asserts! (get is-staked stake-info) err-not-found)
        (asserts! (is-eq (get staker stake-info) tx-sender) err-not-authorized)
        (try! (stx-transfer? reward-amount contract-owner tx-sender))
        (ok (map-set staked-assets
            {asset-id: asset-id}
            (merge stake-info {rewards-claimed: (+ (get rewards-claimed stake-info) reward-amount)})
        ))
    )
)


(define-public (initialize-asset-pricing (asset-id uint) (base-price uint))
    (let ((asset (unwrap! (map-get? gaming-assets {asset-id: asset-id}) err-not-found)))
        (asserts! (is-eq (get owner asset) tx-sender) err-not-authorized)
        (asserts! (> base-price u0) err-invalid-price)
        (map-set asset-price-history
            {asset-id: asset-id}
            {
                base-price: base-price,
                current-multiplier: (var-get base-multiplier),
                last-sale-block: stacks-block-height,
                total-views: u0,
                failed-purchases: u0,
                successful-sales: u0
            }
        )
        (ok (map-set market-demand-metrics
            {asset-id: asset-id}
            {
                view-count-24h: u0,
                purchase-attempts-24h: u0,
                last-metric-reset: stacks-block-height,
                demand-score: u0
            }
        ))
    )
)

(define-public (record-asset-view (asset-id uint))
    (let (
        (current-metrics (default-to
            {view-count-24h: u0, purchase-attempts-24h: u0, last-metric-reset: stacks-block-height, demand-score: u0}
            (map-get? market-demand-metrics {asset-id: asset-id})))
        (price-history (unwrap! (map-get? asset-price-history {asset-id: asset-id}) err-not-found))
        )
        (map-set asset-price-history
            {asset-id: asset-id}
            (merge price-history {total-views: (+ (get total-views price-history) u1)})
        )
        (ok (map-set market-demand-metrics
            {asset-id: asset-id}
            (merge current-metrics {view-count-24h: (+ (get view-count-24h current-metrics) u1)})
        ))
    )
)

(define-public (calculate-dynamic-price (asset-id uint))
    (let (
        (price-history (unwrap! (map-get? asset-price-history {asset-id: asset-id}) err-not-found))
        (demand-metrics (unwrap! (map-get? market-demand-metrics {asset-id: asset-id}) err-not-found))
        (demand-score (calculate-demand-score asset-id))
        (new-multiplier (calculate-price-multiplier demand-score))
        (adjusted-price (/ (* (get base-price price-history) new-multiplier) u1000))
        )
        (asserts! (var-get price-engine-enabled) err-not-authorized)
        (map-set asset-price-history
            {asset-id: asset-id}
            (merge price-history {current-multiplier: new-multiplier})
        )
        (ok adjusted-price)
    )
)

(define-public (update-asset-price-on-sale (asset-id uint) (sale-successful bool))
    (let (
        (price-history (unwrap! (map-get? asset-price-history {asset-id: asset-id}) err-not-found))
        (demand-metrics (unwrap! (map-get? market-demand-metrics {asset-id: asset-id}) err-not-found))
        )
        (if sale-successful
            (begin
                (map-set asset-price-history
                    {asset-id: asset-id}
                    (merge price-history {
                        successful-sales: (+ (get successful-sales price-history) u1),
                        last-sale-block: stacks-block-height
                    })
                )
                (ok true)
            )
            (begin
                (map-set asset-price-history
                    {asset-id: asset-id}
                    (merge price-history {failed-purchases: (+ (get failed-purchases price-history) u1)})
                )
                (map-set market-demand-metrics
                    {asset-id: asset-id}
                    (merge demand-metrics {purchase-attempts-24h: (+ (get purchase-attempts-24h demand-metrics) u1)})
                )
                (ok false)
            )
        )
    )
)

(define-public (reset-daily-metrics (asset-id uint))
    (let (
        (current-metrics (unwrap! (map-get? market-demand-metrics {asset-id: asset-id}) err-not-found))
        (blocks-since-reset (- stacks-block-height (get last-metric-reset current-metrics)))
        )
        (asserts! (>= blocks-since-reset u1440) err-not-authorized)
        (ok (map-set market-demand-metrics
            {asset-id: asset-id}
            {
                view-count-24h: u0,
                purchase-attempts-24h: u0,
                last-metric-reset: stacks-block-height,
                demand-score: (calculate-demand-score asset-id)
            }
        ))
    )
)

(define-public (set-price-adjustment-rule (rule-id uint) (min-mult uint) (max-mult uint) (adj-rate uint) (cooldown uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (map-set price-adjustment-rules
            {rule-id: rule-id}
            {
                min-multiplier: min-mult,
                max-multiplier: max-mult,
                adjustment-rate: adj-rate,
                cooldown-period: cooldown
            }
        ))
    )
)

(define-read-only (get-current-market-price (asset-id uint))
    (let (
        (price-history (map-get? asset-price-history {asset-id: asset-id}))
        )
        (match price-history
            history (some (/ (* (get base-price history) (get current-multiplier history)) u1000))
            none
        )
    )
)

(define-read-only (get-asset-demand-metrics (asset-id uint))
    (map-get? market-demand-metrics {asset-id: asset-id})
)

(define-read-only (get-price-trend (asset-id uint))
    (let (
        (price-history (map-get? asset-price-history {asset-id: asset-id}))
        )
        (match price-history
            history (let (
                (current-mult (get current-multiplier history))
                (base-mult (var-get base-multiplier))
                )
                (if (> current-mult base-mult)
                    "increasing"
                    (if (< current-mult base-mult)
                        "decreasing"
                        "stable"
                    )
                )
            )
            "unknown"
        )
    )
)

(define-private (calculate-demand-score (asset-id uint))
    (let (
        (metrics (default-to
            {view-count-24h: u0, purchase-attempts-24h: u0, last-metric-reset: stacks-block-height, demand-score: u0}
            (map-get? market-demand-metrics {asset-id: asset-id})))
        (price-history (default-to
            {base-price: u0, current-multiplier: u1000, last-sale-block: stacks-block-height, total-views: u0, failed-purchases: u0, successful-sales: u0}
            (map-get? asset-price-history {asset-id: asset-id})))
        (view-weight u3)
        (attempt-weight u5)
        (success-weight u10)
        )
        (+
            (* (get view-count-24h metrics) view-weight)
            (* (get purchase-attempts-24h metrics) attempt-weight)
            (* (get successful-sales price-history) success-weight)
        )
    )
)

(define-private (min (a uint) (b uint))
    (if (< a b) a b)
)

(define-private (max (a uint) (b uint))
    (if (> a b) a b)
)

(define-private (calculate-price-multiplier (demand-score uint))
    (let (
        (base-mult (var-get base-multiplier))
        (high-threshold (var-get demand-threshold-high))
        (low-threshold (var-get demand-threshold-low))
        )
        (if (>= demand-score high-threshold)
            (min (var-get max-price-increase) (+ base-mult (* (- demand-score high-threshold) u50)))
            (if (<= demand-score low-threshold)
                (max (var-get min-price-decrease) (- base-mult (* (- low-threshold demand-score) u25)))
                base-mult
            )
        )
    )
)

(define-public (enable-price-engine (enabled bool))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (var-set price-engine-enabled enabled))
    )
)

(define-public (update-price-parameters (new-base-multiplier uint) (new-max-increase uint) (new-min-decrease uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set base-multiplier new-base-multiplier)
        (var-set max-price-increase new-max-increase)
        (var-set min-price-decrease new-min-decrease)
        (ok true)
    )
)