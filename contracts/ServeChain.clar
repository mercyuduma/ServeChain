;; ServeChain: Community Service and Volunteering System
;; Version: 1.0.0

;; Constants
(define-constant COMMUNITY_CENTER_CAPACITY u4200000)
(define-constant BASE_SERVICE_REWARD u46)
(define-constant IMPACT_BONUS u28)
(define-constant MAX_VOLUNTEER_LEVEL u34)
(define-constant ERR_INVALID_SERVICE_SESSION u1)
(define-constant ERR_NO_SERVICE_TOKENS u2)
(define-constant ERR_CENTER_CAPACITY_EXCEEDED u3)
(define-constant BLOCKS_PER_SERVICE_CYCLE u3240)
(define-constant PROJECT_MULTIPLIER u19)
(define-constant MIN_PROJECT_PERIOD u1620)
(define-constant SERVICE_LAPSE_PENALTY u44)

;; Data Variables
(define-data-var total-service-tokens-distributed uint u0)
(define-data-var total-service-sessions uint u0)
(define-data-var community-coordinator principal tx-sender)

;; Data Maps
(define-map volunteer-sessions principal uint)
(define-map volunteer-service-tokens principal uint)
(define-map service-session-start-time principal uint)
(define-map volunteer-impact-level principal uint)
(define-map volunteer-last-session principal uint)
(define-map volunteer-service-project principal uint)
(define-map volunteer-project-start-block principal uint)
(define-map service-complexity principal uint)
(define-map volunteer-achievement-count principal uint)
(define-map service-specialization principal uint)

;; Public Functions
(define-public (start-service-session (service-area uint) (complexity-level uint))
  (let
    (
      (volunteer tx-sender)
    )
    (asserts! (and (> service-area u0) (> complexity-level u0) (<= complexity-level u14)) (err ERR_INVALID_SERVICE_SESSION))
    (map-set service-session-start-time volunteer burn-block-height)
    (map-set service-complexity volunteer complexity-level)
    (ok true)
  ))

(define-public (complete-service-session (service-area uint) (impact-score uint))
  (let
    (
      (volunteer tx-sender)
      (start-block (default-to u0 (map-get? service-session-start-time volunteer)))
      (blocks-serving (- burn-block-height start-block))
      (last-session-block (default-to u0 (map-get? volunteer-last-session volunteer)))
      (impact-level (default-to u0 (map-get? volunteer-impact-level volunteer)))
      (capped-impact (if (<= impact-level MAX_VOLUNTEER_LEVEL) impact-level MAX_VOLUNTEER_LEVEL))
      (impact-bonus-calc (/ (* impact-score u18) u100))
      (specialization-bonus (default-to u0 (map-get? service-specialization volunteer)))
      (service-reward (+ BASE_SERVICE_REWARD (* capped-impact IMPACT_BONUS) impact-bonus-calc specialization-bonus))
    )
    (asserts! (and (> start-block u0) (>= blocks-serving service-area) (<= impact-score u100)) (err ERR_INVALID_SERVICE_SESSION))
    
    (map-set volunteer-sessions volunteer (+ (default-to u0 (map-get? volunteer-sessions volunteer)) u1))
    (map-set volunteer-service-tokens volunteer (+ (default-to u0 (map-get? volunteer-service-tokens volunteer)) service-reward))
    
    (if (< (- burn-block-height last-session-block) BLOCKS_PER_SERVICE_CYCLE)
      (map-set volunteer-impact-level volunteer (+ impact-level u1))
      (map-set volunteer-impact-level volunteer u1)
    )
    
    (if (>= impact-score u94)
      (map-set service-specialization volunteer (+ specialization-bonus u11))
      true
    )
    
    (map-set volunteer-last-session volunteer burn-block-height)
    (var-set total-service-sessions (+ (var-get total-service-sessions) u1))
    (var-set total-service-tokens-distributed (+ (var-get total-service-tokens-distributed) service-reward))
    
    (asserts! (<= (var-get total-service-tokens-distributed) COMMUNITY_CENTER_CAPACITY) (err ERR_CENTER_CAPACITY_EXCEEDED))
    (ok service-reward)
  ))

(define-public (claim-service-rewards)
  (let
    (
      (volunteer tx-sender)
      (token-balance (default-to u0 (map-get? volunteer-service-tokens volunteer)))
    )
    (asserts! (> token-balance u0) (err ERR_NO_SERVICE_TOKENS))
    (map-set volunteer-service-tokens volunteer u0)
    (ok token-balance)
  ))

;; Service Project Features
(define-public (start-service-project (project-scope uint))
  (let
    (
      (volunteer tx-sender)
    )
    (asserts! (> project-scope u0) (err ERR_INVALID_SERVICE_SESSION))
    (asserts! (>= (var-get total-service-tokens-distributed) project-scope) (err ERR_CENTER_CAPACITY_EXCEEDED))
    
    (map-set volunteer-service-project volunteer project-scope)
    (map-set volunteer-project-start-block volunteer burn-block-height)
    (var-set total-service-tokens-distributed (- (var-get total-service-tokens-distributed) project-scope))
    (ok project-scope)
  ))

(define-public (complete-service-project)
  (let
    (
      (volunteer tx-sender)
      (project-amount (default-to u0 (map-get? volunteer-service-project volunteer)))
      (project-start-block (default-to u0 (map-get? volunteer-project-start-block volunteer)))
      (blocks-serving (- burn-block-height project-start-block))
      (penalty (if (< blocks-serving MIN_PROJECT_PERIOD) (/ (* project-amount SERVICE_LAPSE_PENALTY) u100) u0))
      (project-bonus (if (>= blocks-serving MIN_PROJECT_PERIOD) (/ (* project-amount PROJECT_MULTIPLIER) u100) u0))
      (final-amount (+ (- project-amount penalty) project-bonus))
    )
    (asserts! (> project-amount u0) (err ERR_NO_SERVICE_TOKENS))
    
    (map-set volunteer-service-project volunteer u0)
    (map-set volunteer-project-start-block volunteer u0)
    (map-set volunteer-achievement-count volunteer (+ (default-to u0 (map-get? volunteer-achievement-count volunteer)) u1))
    (var-set total-service-tokens-distributed (+ (var-get total-service-tokens-distributed) final-amount))
    (ok final-amount)
  ))

(define-public (publish-community-achievement (achievement-quality uint) (community-validation uint))
  (let
    (
      (volunteer tx-sender)
      (impact-level (default-to u0 (map-get? volunteer-impact-level volunteer)))
      (achievement-count (default-to u0 (map-get? volunteer-achievement-count volunteer)))
      (achievement-bonus (+ (* achievement-quality u26) (* community-validation u24) (* achievement-count u16)))
    )
    (asserts! (and (> achievement-quality u0) (> community-validation u0) (>= impact-level u14)) (err ERR_INVALID_SERVICE_SESSION))
    
    (map-set volunteer-service-tokens volunteer (+ (default-to u0 (map-get? volunteer-service-tokens volunteer)) achievement-bonus))
    (var-set total-service-tokens-distributed (+ (var-get total-service-tokens-distributed) achievement-bonus))
    
    (ok achievement-bonus)
  ))

(define-public (coordinate-volunteer-programs (volunteer-count uint) (coordination-hours uint))
  (let
    (
      (volunteer tx-sender)
      (impact-level (default-to u0 (map-get? volunteer-impact-level volunteer)))
      (specialization-level (default-to u0 (map-get? service-specialization volunteer)))
      (coordination-bonus (+ (* volunteer-count u42) (* coordination-hours u11) (* specialization-level u6)))
    )
    (asserts! (and (> volunteer-count u0) (> coordination-hours u0) (>= impact-level u19)) (err ERR_INVALID_SERVICE_SESSION))
    
    (map-set volunteer-service-tokens volunteer (+ (default-to u0 (map-get? volunteer-service-tokens volunteer)) coordination-bonus))
    (var-set total-service-tokens-distributed (+ (var-get total-service-tokens-distributed) coordination-bonus))
    
    (ok coordination-bonus)
  ))

;; Read-Only Functions
(define-read-only (get-service-session-count (user principal))
  (default-to u0 (map-get? volunteer-sessions user)))

(define-read-only (get-service-token-balance (user principal))
  (default-to u0 (map-get? volunteer-service-tokens user)))

(define-read-only (get-impact-level (user principal))
  (default-to u0 (map-get? volunteer-impact-level user)))

(define-read-only (get-achievement-count (user principal))
  (default-to u0 (map-get? volunteer-achievement-count user)))

(define-read-only (get-service-project (user principal))
  (default-to u0 (map-get? volunteer-service-project user)))

(define-read-only (get-service-specialization (user principal))
  (default-to u0 (map-get? service-specialization user)))

(define-read-only (get-community-center-stats)
  {
    total-service-sessions: (var-get total-service-sessions),
    total-service-tokens-distributed: (var-get total-service-tokens-distributed),
    community-center-capacity: COMMUNITY_CENTER_CAPACITY
  })

(define-read-only (calculate-service-reward (impact-level uint) (impact-score uint) (specialization-bonus uint))
  (let
    (
      (capped-impact (if (<= impact-level MAX_VOLUNTEER_LEVEL) impact-level MAX_VOLUNTEER_LEVEL))
      (impact-bonus-calc (/ (* impact-score u18) u100))
    )
    (+ BASE_SERVICE_REWARD (* capped-impact IMPACT_BONUS) impact-bonus-calc specialization-bonus)
  ))

;; Private Functions
(define-private (is-community-coordinator)
  (is-eq tx-sender (var-get community-coordinator)))

(define-private (validate-service-parameters (service-area uint) (impact-score uint))
  (and (> service-area u0) (<= impact-score u100)))