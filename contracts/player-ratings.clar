;; Player Rating and Leaderboards System
;; Tracks player skill ratings across game categories with seasonal leaderboards

;; Error constants
(define-constant ERR-UNAUTHORIZED (err u700))
(define-constant ERR-NOT-FOUND (err u701))
(define-constant ERR-INVALID-CATEGORY (err u702))
(define-constant ERR-INVALID-RESULT (err u703))
(define-constant ERR-SEASON-ENDED (err u704))
(define-constant ERR-INSUFFICIENT-FUNDS (err u705))
(define-constant ERR-ALREADY-CLAIMED (err u706))

;; Game categories for different rating pools
(define-constant CATEGORY-STRATEGY u1)
(define-constant CATEGORY-ACTION u2)
(define-constant CATEGORY-PUZZLE u3)
(define-constant CATEGORY-SPORTS u4)
(define-constant CATEGORY-CARD u5)

;; Rating constants
(define-constant DEFAULT-RATING u1200)
(define-constant K-FACTOR u32) ;; ELO K-factor for rating changes
(define-constant MIN-RATING u100)
(define-constant MAX-RATING u3000)

;; Season constants
(define-constant SEASON-LENGTH u43200) ;; ~30 days in blocks
(define-constant LEADERBOARD-SIZE u50) ;; Top 50 players tracked
(define-constant MIN-MATCHES-FOR-REWARDS u5) ;; Minimum matches to earn rewards

;; Contract owner and global variables
(define-data-var contract-owner principal tx-sender)
(define-data-var current-season uint u1)
(define-data-var season-start-block uint u0)
(define-data-var total-prize-pool uint u0)

;; Player ratings by category and season
(define-map player-ratings
  { player: principal, category: uint, season: uint }
  {
    rating: uint,
    matches-played: uint,
    wins: uint,
    losses: uint,
    last-match-block: uint,
    peak-rating: uint,
    rating-change: int ;; Last rating change for display
  }
)

;; Season leaderboards tracking top performers
(define-map season-leaderboards
  { season: uint, category: uint, rank: uint }
  {
    player: principal,
    final-rating: uint,
    total-matches: uint,
    win-percentage: uint
  }
)

;; Season metadata and prize distribution
(define-map season-info
  uint ;; season number
  {
    start-block: uint,
    end-block: uint,
    total-prize-pool: uint,
    total-participants: uint,
    completed: bool,
    prizes-distributed: bool
  }
)

;; Prize claims tracking
(define-map prize-claims
  { player: principal, season: uint }
  {
    category: uint,
    rank: uint,
    prize-amount: uint,
    claimed: bool,
    claim-block: (optional uint)
  }
)

;; Match results for rating calculations
(define-map match-results
  { match-id: uint }
  {
    player1: principal,
    player2: principal,
    winner: principal,
    category: uint,
    season: uint,
    recorded-at: uint,
    rating-change-p1: int,
    rating-change-p2: int
  }
)

;; Match counter
(define-data-var match-counter uint u0)

;; Initialize current season
(define-public (initialize-season)
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-UNAUTHORIZED)
    (var-set season-start-block stacks-block-height)
    (map-set season-info (var-get current-season)
      {
        start-block: stacks-block-height,
        end-block: (+ stacks-block-height SEASON-LENGTH),
        total-prize-pool: u0,
        total-participants: u0,
        completed: false,
        prizes-distributed: false
      }
    )
    (ok true)
  )
)

;; Record match result and update ratings
(define-public (record-match-result (player1 principal) (player2 principal) (winner principal) (category uint))
  (let (
    (match-id (+ (var-get match-counter) u1))
    (current-season-num (var-get current-season))
    (p1-rating (get-player-rating player1 category current-season-num))
    (p2-rating (get-player-rating player2 category current-season-num))
    (rating-changes (calculate-rating-change p1-rating p2-rating (is-eq winner player1)))
  )
    ;; Validate inputs
    (asserts! (and (>= category CATEGORY-STRATEGY) (<= category CATEGORY-CARD)) ERR-INVALID-CATEGORY)
    (asserts! (or (is-eq winner player1) (is-eq winner player2)) ERR-INVALID-RESULT)
    (asserts! (not (is-eq player1 player2)) ERR-INVALID-RESULT)
    (asserts! (not (get completed (get-season-info current-season-num))) ERR-SEASON-ENDED)
    
    ;; Update match counter
    (var-set match-counter match-id)
    
    ;; Record match
    (map-set match-results { match-id: match-id }
      {
        player1: player1,
        player2: player2,
        winner: winner,
        category: category,
        season: current-season-num,
        recorded-at: stacks-block-height,
        rating-change-p1: (get p1-change rating-changes),
        rating-change-p2: (get p2-change rating-changes)
      }
    )
    
    ;; Update player ratings
    (try! (update-player-rating player1 category current-season-num (get p1-change rating-changes) (is-eq winner player1)))
    (try! (update-player-rating player2 category current-season-num (get p2-change rating-changes) (is-eq winner player2)))
    
    (ok match-id)
  )
)

;; Add funds to season prize pool (anyone can contribute)
(define-public (contribute-to-prize-pool (amount uint))
  (let (
    (current-season-num (var-get current-season))
    (season (get-season-info current-season-num))
  )
    (asserts! (> amount u0) ERR-INSUFFICIENT-FUNDS)
    (asserts! (not (get completed season)) ERR-SEASON-ENDED)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set season-info current-season-num
      (merge season { total-prize-pool: (+ (get total-prize-pool season) amount) })
    )
    (var-set total-prize-pool (+ (var-get total-prize-pool) amount))
    (ok true)
  )
)

;; End current season and calculate final rankings
(define-public (end-season)
  (let (
    (current-season-num (var-get current-season))
    (season (get-season-info current-season-num))
  )
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-UNAUTHORIZED)
    (asserts! (>= stacks-block-height (get end-block season)) ERR-SEASON-ENDED)
    (asserts! (not (get completed season)) ERR-SEASON-ENDED)
    
    ;; Mark season as completed
    (map-set season-info current-season-num (merge season { completed: true }))
    
    ;; Start new season
    (var-set current-season (+ current-season-num u1))
    (var-set season-start-block stacks-block-height)
    (map-set season-info (var-get current-season)
      {
        start-block: stacks-block-height,
        end-block: (+ stacks-block-height SEASON-LENGTH),
        total-prize-pool: u0,
        total-participants: u0,
        completed: false,
        prizes-distributed: false
      }
    )
    
    (ok true)
  )
)

;; Claim season-end rewards (top performers)
(define-public (claim-season-reward (season uint) (category uint) (claimed-rank uint))
  (let (
    (season-data (get-season-info season))
    (prize-data (default-to 
      { category: u0, rank: u0, prize-amount: u0, claimed: false, claim-block: none }
      (map-get? prize-claims { player: tx-sender, season: season })
    ))
  )
    (asserts! (get completed season-data) ERR-SEASON-ENDED)
    (asserts! (not (get claimed prize-data)) ERR-ALREADY-CLAIMED)
    
    ;; Calculate prize amount based on rank (simplified distribution)
    (let ((prize-amount (calculate-prize-amount season category claimed-rank)))
      (asserts! (> prize-amount u0) ERR-NOT-FOUND)
      (try! (as-contract (stx-transfer? prize-amount tx-sender tx-sender)))
      (map-set prize-claims { player: tx-sender, season: season }
        {
          category: category,
          rank: claimed-rank,
          prize-amount: prize-amount,
          claimed: true,
          claim-block: (some stacks-block-height)
        }
      )
      (ok prize-amount)
    )
  )
)

;; Private helper functions

(define-private (get-player-rating (player principal) (category uint) (season uint))
  (let ((rating-data (map-get? player-ratings { player: player, category: category, season: season })))
    (match rating-data
      data (get rating data)
      DEFAULT-RATING
    )
  )
)

(define-private (update-player-rating (player principal) (category uint) (season uint) (rating-change int) (won bool))
  (let (
    (current-data (default-to
      {
        rating: DEFAULT-RATING,
        matches-played: u0,
        wins: u0,
        losses: u0,
        last-match-block: u0,
        peak-rating: DEFAULT-RATING,
        rating-change: 0
      }
      (map-get? player-ratings { player: player, category: category, season: season })
    ))
    (new-rating (max MIN-RATING (min MAX-RATING (safe-to-uint (+ (unwrap-panic (to-int (get rating current-data))) rating-change)))))
    (new-peak (max (get peak-rating current-data) new-rating))
  )
    (map-set player-ratings { player: player, category: category, season: season }
      {
        rating: new-rating,
        matches-played: (+ (get matches-played current-data) u1),
        wins: (if won (+ (get wins current-data) u1) (get wins current-data)),
        losses: (if won (get losses current-data) (+ (get losses current-data) u1)),
        last-match-block: stacks-block-height,
        peak-rating: new-peak,
        rating-change: rating-change
      }
    )
    (ok true)
  )
)

(define-private (calculate-rating-change (rating1 uint) (rating2 uint) (player1-won bool))
  (let (
    (rating-diff (if (> rating2 rating1) (- rating2 rating1) (- rating1 rating2)))
    (rating-factor (if (> rating-diff u400) u400 rating-diff))
    (base-change (/ (* K-FACTOR rating-factor) u400))
  )
    ;; Simplified ELO calculation returning signed integers
    (if player1-won
      ;; Player 1 won: gains points, player 2 loses points
      (if (> rating2 rating1)
        ;; Underdog wins: bigger gain
        { p1-change: (unwrap-panic (to-int (+ base-change u5))), p2-change: (- 0 (unwrap-panic (to-int (+ base-change u5)))) }
        ;; Favorite wins: smaller gain  
        { p1-change: (unwrap-panic (to-int base-change)), p2-change: (- 0 (unwrap-panic (to-int base-change))) }
      )
      ;; Player 2 won: player 1 loses points, player 2 gains points
      (if (> rating1 rating2)
        ;; Underdog wins: bigger gain for player 2
        { p1-change: (- 0 (unwrap-panic (to-int (+ base-change u5)))), p2-change: (unwrap-panic (to-int (+ base-change u5))) }
        ;; Favorite wins: smaller gain for player 2
        { p1-change: (- 0 (unwrap-panic (to-int base-change))), p2-change: (unwrap-panic (to-int base-change)) }
      )
    )
  )
)

(define-private (calculate-prize-amount (season uint) (category uint) (rank uint))
  (let ((season-data (get-season-info season)))
    (if (<= rank u3)
      (let ((total-pool (get total-prize-pool season-data)))
        (if (is-eq rank u1)
          (/ (* total-pool u50) u100) ;; 50% for 1st place
          (if (is-eq rank u2)
            (/ (* total-pool u30) u100) ;; 30% for 2nd place
            (/ (* total-pool u20) u100) ;; 20% for 3rd place
          )
        )
      )
      u0
    )
  )
)

;; Utility functions
(define-private (max (a uint) (b uint)) (if (> a b) a b))
(define-private (min (a uint) (b uint)) (if (< a b) a b))
(define-private (safe-to-uint (value int))
  (if (>= value 0) (unwrap-panic (to-uint value)) u0)
)

;; Read-only functions

(define-read-only (get-player-rating-info (player principal) (category uint) (season uint))
  (map-get? player-ratings { player: player, category: category, season: season })
)

(define-read-only (get-current-season) (var-get current-season))

(define-read-only (get-season-info (season uint))
  (default-to
    { start-block: u0, end-block: u0, total-prize-pool: u0, total-participants: u0, completed: false, prizes-distributed: false }
    (map-get? season-info season)
  )
)

(define-read-only (get-match-result (match-id uint))
  (map-get? match-results { match-id: match-id })
)

(define-read-only (get-season-leaderboard (season uint) (category uint) (rank uint))
  (map-get? season-leaderboards { season: season, category: category, rank: rank })
)

(define-read-only (get-prize-claim (player principal) (season uint))
  (map-get? prize-claims { player: player, season: season })
)
