;; Tournament Bracket Management System
;; Handles bracket generation, match tracking, and prize distribution

;; Error constants
(define-constant err-owner-only (err u300))
(define-constant err-not-found (err u301))
(define-constant err-unauthorized (err u302))
(define-constant err-invalid-state (err u303))
(define-constant err-insufficient-participants (err u304))
(define-constant err-bracket-locked (err u305))
(define-constant err-match-not-ready (err u306))
(define-constant err-invalid-result (err u307))

;; Constants
(define-constant contract-owner tx-sender)
(define-constant min-tournament-participants u4)
(define-constant max-tournament-participants u16)
(define-constant bracket-lock-delay u144) ;; ~1 day to finalize brackets

;; Tournament bracket structure
(define-map tournament-brackets
  { tournament-id: uint }
  {
    organizer: principal,
    total-participants: uint,
    bracket-size: uint, ;; Next power of 2 >= participants
    current-round: uint,
    max-rounds: uint,
    prize-pool: uint,
    status: (string-ascii 20), ;; "setup", "active", "completed"
    winner: (optional principal),
    created-at: uint,
    locked-at: uint
  }
)

;; Individual match data within tournaments
(define-map tournament-matches
  { tournament-id: uint, round: uint, match-id: uint }
  {
    player1: (optional principal),
    player2: (optional principal), 
    winner: (optional principal),
    score1: uint,
    score2: uint,
    completed: bool,
    submitted-at: uint,
    reporter: (optional principal)
  }
)

;; Track participant placement and earnings
(define-map tournament-results
  { tournament-id: uint, participant: principal }
  {
    final-placement: uint,
    rounds-survived: uint,
    prize-earned: uint,
    paid: bool
  }
)

;; Prize distribution percentages for different placements
(define-map prize-distributions
  uint ;; placement (1st, 2nd, 3rd, etc.)
  { percentage: uint } ;; percentage of total prize pool
)

;; Global tournament counter
(define-data-var tournament-counter uint u0)

;; Initialize default prize distribution (contract owner only)
(define-public (initialize-prize-distribution)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set prize-distributions u1 { percentage: u50 }) ;; 1st place: 50%
    (map-set prize-distributions u2 { percentage: u30 }) ;; 2nd place: 30%
    (map-set prize-distributions u3 { percentage: u15 }) ;; 3rd place: 15%
    (map-set prize-distributions u4 { percentage: u5 })  ;; 4th place: 5%
    (ok true)
  )
)

;; Create tournament bracket from registered participants
(define-public (create-tournament-bracket (tournament-id uint) (participants (list 16 principal)) (prize-pool uint))
  (let (
    (participant-count (len participants))
    (bracket-size (calculate-bracket-size participant-count))
    (max-rounds (calculate-max-rounds bracket-size))
    (tournament-counter-val (+ (var-get tournament-counter) u1))
  )
    (asserts! (>= participant-count min-tournament-participants) err-insufficient-participants)
    (asserts! (<= participant-count max-tournament-participants) err-insufficient-participants)
    (asserts! (> prize-pool u0) err-invalid-state)
    
    ;; Transfer prize pool to contract
    (try! (stx-transfer? prize-pool tx-sender (as-contract tx-sender)))
    
    ;; Create tournament bracket
    (map-set tournament-brackets
      { tournament-id: tournament-id }
      {
        organizer: tx-sender,
        total-participants: participant-count,
        bracket-size: bracket-size,
        current-round: u1,
        max-rounds: max-rounds,
        prize-pool: prize-pool,
        status: "setup",
        winner: none,
        created-at: stacks-block-height,
        locked-at: u0
      }
    )
    
    ;; Generate first round matches
    (unwrap-panic (generate-first-round-matches tournament-id participants bracket-size))
    
    (var-set tournament-counter tournament-counter-val)
    (ok tournament-id)
  )
)

;; Submit match result (called by either participant)
(define-public (submit-match-result 
  (tournament-id uint) 
  (round uint) 
  (match-id uint) 
  (score1 uint) 
  (score2 uint))
  (let (
    (match-data (unwrap! (map-get? tournament-matches { tournament-id: tournament-id, round: round, match-id: match-id }) err-not-found))
    (tournament (unwrap! (map-get? tournament-brackets { tournament-id: tournament-id }) err-not-found))
  )
    ;; Validate submission
    (asserts! (is-eq (get status tournament) "active") err-invalid-state)
    (asserts! (or (is-eq (some tx-sender) (get player1 match-data))
                 (is-eq (some tx-sender) (get player2 match-data))) err-unauthorized)
    (asserts! (not (get completed match-data)) err-invalid-state)
    (asserts! (not (is-eq score1 score2)) err-invalid-result) ;; No ties allowed
    
    ;; Determine winner
    (let ((winner (if (> score1 score2) (get player1 match-data) (get player2 match-data))))
      ;; Update match with results
      (map-set tournament-matches
        { tournament-id: tournament-id, round: round, match-id: match-id }
        (merge match-data {
          winner: winner,
          score1: score1,
          score2: score2,
          completed: true,
          submitted-at: stacks-block-height,
          reporter: (some tx-sender)
        })
      )
      
      ;; Check if round is complete and advance tournament
      (try! (check-and-advance-tournament tournament-id round))
      (ok true)
    )
  )
)

;; Lock bracket and start tournament (organizer only)
(define-public (start-tournament (tournament-id uint))
  (let (
    (tournament (unwrap! (map-get? tournament-brackets { tournament-id: tournament-id }) err-not-found))
  )
    (asserts! (is-eq tx-sender (get organizer tournament)) err-owner-only)
    (asserts! (is-eq (get status tournament) "setup") err-invalid-state)
    
    (map-set tournament-brackets
      { tournament-id: tournament-id }
      (merge tournament {
        status: "active",
        locked-at: stacks-block-height
      })
    )
    (ok true)
  )
)

;; Claim tournament prize (winners only)
(define-public (claim-tournament-prize (tournament-id uint))
  (let (
    (tournament (unwrap! (map-get? tournament-brackets { tournament-id: tournament-id }) err-not-found))
    (result (unwrap! (map-get? tournament-results { tournament-id: tournament-id, participant: tx-sender }) err-not-found))
  )
    (asserts! (is-eq (get status tournament) "completed") err-invalid-state)
    (asserts! (not (get paid result)) err-invalid-state)
    (asserts! (> (get prize-earned result) u0) err-not-found)
    
    ;; Transfer prize money
    (try! (as-contract (stx-transfer? (get prize-earned result) tx-sender tx-sender)))
    
    ;; Mark as paid
    (map-set tournament-results
      { tournament-id: tournament-id, participant: tx-sender }
      (merge result { paid: true })
    )
    
    (ok (get prize-earned result))
  )
)

;; Helper function to calculate bracket size (next power of 2)
(define-private (calculate-bracket-size (participants uint))
  (if (<= participants u4) u4
    (if (<= participants u8) u8
      (if (<= participants u16) u16 u16)
    )
  )
)

;; Helper function to calculate maximum rounds
(define-private (calculate-max-rounds (bracket-size uint))
  (if (is-eq bracket-size u4) u2
    (if (is-eq bracket-size u8) u3
      (if (is-eq bracket-size u16) u4 u4)
    )
  )
)

;; Generate first round matches
(define-private (generate-first-round-matches (tournament-id uint) (participants (list 16 principal)) (bracket-size uint))
  (begin
    ;; Generate match pairings for first round (simplified for brevity)
    (map-set tournament-matches
      { tournament-id: tournament-id, round: u1, match-id: u1 }
      {
        player1: (element-at participants u0),
        player2: (element-at participants u1),
        winner: none, score1: u0, score2: u0, completed: false,
        submitted-at: u0, reporter: none
      }
    )
    ;; Additional matches would be generated in a full implementation
    (ok true)
  )
)

;; Check if current round is complete and advance tournament
(define-private (check-and-advance-tournament (tournament-id uint) (round uint))
  (let (
    (tournament (unwrap! (map-get? tournament-brackets { tournament-id: tournament-id }) err-not-found))
  )
    ;; Simplified: assume round complete after any match (full implementation would check all matches)
    (if (is-eq round (get max-rounds tournament))
      ;; Tournament complete - distribute prizes
      (begin
        (try! (distribute-tournament-prizes tournament-id))
        (map-set tournament-brackets
          { tournament-id: tournament-id }
          (merge tournament { status: "completed" })
        )
      )
      ;; Advance to next round
      (map-set tournament-brackets
        { tournament-id: tournament-id }
        (merge tournament { current-round: (+ round u1) })
      )
    )
    (ok true)
  )
)

;; Distribute prizes based on final placements
(define-private (distribute-tournament-prizes (tournament-id uint))
  (let (
    (tournament (unwrap! (map-get? tournament-brackets { tournament-id: tournament-id }) err-not-found))
    (total-prize (get prize-pool tournament))
  )
    ;; Simplified prize distribution (full implementation would calculate all placements)
    ;; Award winner 50% of prize pool
    (match (get winner tournament)
      winner-addr (begin
        (map-set tournament-results
          { tournament-id: tournament-id, participant: winner-addr }
          {
            final-placement: u1,
            rounds-survived: (get max-rounds tournament),
            prize-earned: (/ (* total-prize u50) u100),
            paid: false
          }
        )
      )
      true
    )
    (ok true)
  )
)

;; Read-only functions
(define-read-only (get-tournament-bracket (tournament-id uint))
  (map-get? tournament-brackets { tournament-id: tournament-id })
)

(define-read-only (get-tournament-match (tournament-id uint) (round uint) (match-id uint))
  (map-get? tournament-matches { tournament-id: tournament-id, round: round, match-id: match-id })
)

(define-read-only (get-tournament-result (tournament-id uint) (participant principal))
  (map-get? tournament-results { tournament-id: tournament-id, participant: participant })
)

(define-read-only (get-prize-distribution (placement uint))
  (map-get? prize-distributions placement)
)
