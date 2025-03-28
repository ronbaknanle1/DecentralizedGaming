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