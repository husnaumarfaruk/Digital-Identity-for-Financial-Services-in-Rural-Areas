;; transaction-history.clar
;; Builds financial reputation for credit access

;; Store service provider information locally
(define-map service-providers
  { id: uint }
  {
    provider: principal,
    active: bool
  }
)

;; Store service access information locally
(define-map service-access
  { identity-id: uint, service-id: uint }
  { granted: bool }
)

(define-map transactions
  { id: uint }
  {
    identity-id: uint,
    amount: uint,
    transaction-type: (string-utf8 20),
    timestamp: uint,
    service-id: uint
  }
)

(define-map credit-scores
  { identity-id: uint }
  {
    score: uint,
    transaction-count: uint,
    last-updated: uint
  }
)

(define-data-var next-transaction-id uint u1)

(define-constant INITIAL_CREDIT_SCORE u300)
(define-constant MAX_CREDIT_SCORE u850)
(define-constant TRANSACTION_SCORE_INCREASE u5)

;; Register a service provider (to be called after registering in service-access contract)
(define-public (register-service-provider (id uint) (provider principal))
  (begin
    (map-set service-providers
      { id: id }
      {
        provider: provider,
        active: true
      }
    )
    (ok true)
  )
)

;; Update service access (to be called after granting/revoking access in service-access contract)
(define-public (update-service-access (identity-id uint) (service-id uint) (granted bool))
  (begin
    (map-set service-access
      { identity-id: identity-id, service-id: service-id }
      { granted: granted }
    )
    (ok true)
  )
)

(define-public (record-transaction (identity-id uint) (amount uint) (transaction-type (string-utf8 20)) (service-id uint))
  (let
    (
      (new-id (var-get next-transaction-id))
      (service (unwrap! (map-get? service-providers { id: service-id }) (err u1))) ;; Service not found
      (access (default-to { granted: false } (map-get? service-access { identity-id: identity-id, service-id: service-id })))
    )
    (asserts! (is-eq tx-sender (get provider service)) (err u3)) ;; Not the service provider
    (asserts! (get granted access) (err u4)) ;; Access not granted

    (map-set transactions
      { id: new-id }
      {
        identity-id: identity-id,
        amount: amount,
        transaction-type: transaction-type,
        timestamp: block-height,
        service-id: service-id
      }
    )
    (var-set next-transaction-id (+ new-id u1))
    (update-credit-score identity-id)
    (ok new-id)
  )
)

(define-private (update-credit-score (identity-id uint))
  (let
    (
      (current-score (default-to
        { score: INITIAL_CREDIT_SCORE, transaction-count: u0, last-updated: u0 }
        (map-get? credit-scores { identity-id: identity-id })))
      (new-count (+ (get transaction-count current-score) u1))
      (score-increase (+ (get score current-score) TRANSACTION_SCORE_INCREASE))
      (new-score (if (> score-increase MAX_CREDIT_SCORE)
                    MAX_CREDIT_SCORE
                    score-increase))
    )
    (map-set credit-scores
      { identity-id: identity-id }
      {
        score: new-score,
        transaction-count: new-count,
        last-updated: block-height
      }
    )
    true
  )
)

(define-read-only (get-credit-score (identity-id uint))
  (default-to
    { score: INITIAL_CREDIT_SCORE, transaction-count: u0, last-updated: u0 }
    (map-get? credit-scores { identity-id: identity-id })
  )
)

(define-read-only (get-transaction (id uint))
  (map-get? transactions { id: id })
)

(define-read-only (get-transaction-count-for-identity (identity-id uint))
  (let
    ((score-data (get-credit-score identity-id)))
    (get transaction-count score-data)
  )
)
