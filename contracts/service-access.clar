;; service-access.clar
;; Connects users with appropriate financial services

;; Store identity information locally to avoid contract dependencies
(define-map identities
  { id: uint }
  {
    owner: principal,
    active: bool
  }
)

;; Store verification information locally
(define-map verification-counts
  { identity-id: uint }
  { count: uint }
)

(define-map service-providers
  { id: uint }
  {
    name: (string-utf8 100),
    provider: principal,
    active: bool,
    min-verification-level: uint
  }
)

(define-map service-access
  { identity-id: uint, service-id: uint }
  {
    granted: bool,
    access-time: uint
  }
)

(define-data-var next-service-id uint u1)

;; Register an identity (to be called after creating identity in identity-creation contract)
(define-public (register-identity (id uint) (owner principal))
  (begin
    (map-set identities
      { id: id }
      {
        owner: owner,
        active: true
      }
    )
    (ok true)
  )
)

;; Update verification count (to be called after verification in local-verification contract)
(define-public (update-verification-count (identity-id uint) (count uint))
  (begin
    (map-set verification-counts
      { identity-id: identity-id }
      { count: count }
    )
    (ok true)
  )
)

(define-public (register-service-provider (name (string-utf8 100)) (min-verification-level uint))
  (let
    (
      (new-id (var-get next-service-id))
    )
    (map-set service-providers
      { id: new-id }
      {
        name: name,
        provider: tx-sender,
        active: true,
        min-verification-level: min-verification-level
      }
    )
    (var-set next-service-id (+ new-id u1))
    (ok new-id)
  )
)

(define-public (request-service-access (identity-id uint) (service-id uint))
  (let
    (
      (service (unwrap! (map-get? service-providers { id: service-id }) (err u1))) ;; Service not found
      (identity (unwrap! (map-get? identities { id: identity-id }) (err u2))) ;; Identity not found
      (verification-data (default-to { count: u0 } (map-get? verification-counts { identity-id: identity-id })))
      (verification-count (get count verification-data))
    )
    (asserts! (get active service) (err u3)) ;; Service not active
    (asserts! (get active identity) (err u4)) ;; Identity not active
    (asserts! (is-eq (get owner identity) tx-sender) (err u5)) ;; Not the identity owner
    (asserts! (>= verification-count (get min-verification-level service)) (err u6)) ;; Not enough verifications

    (map-set service-access
      { identity-id: identity-id, service-id: service-id }
      {
        granted: true,
        access-time: block-height
      }
    )
    (ok true)
  )
)

(define-read-only (check-service-access (identity-id uint) (service-id uint))
  (default-to
    { granted: false, access-time: u0 }
    (map-get? service-access { identity-id: identity-id, service-id: service-id })
  )
)

(define-public (revoke-service-access (identity-id uint) (service-id uint))
  (let
    (
      (service (unwrap! (map-get? service-providers { id: service-id }) (err u1))) ;; Service not found
      (access (unwrap! (map-get? service-access { identity-id: identity-id, service-id: service-id }) (err u2))) ;; Access not found
      (identity (unwrap! (map-get? identities { id: identity-id }) (err u3))) ;; Identity not found
    )
    (asserts! (or (is-eq tx-sender (get provider service)) (is-eq tx-sender (get owner identity))) (err u4)) ;; Not authorized

    (map-set service-access
      { identity-id: identity-id, service-id: service-id }
      (merge access { granted: false })
    )
    (ok true)
  )
)

;; Update identity status (to be called when identity is deactivated/reactivated)
(define-public (update-identity-status (id uint) (active bool))
  (let
    (
      (identity (unwrap! (map-get? identities { id: id }) (err u1))) ;; Identity not found
    )
    (map-set identities
      { id: id }
      (merge identity { active: active })
    )
    (ok true)
  )
)
