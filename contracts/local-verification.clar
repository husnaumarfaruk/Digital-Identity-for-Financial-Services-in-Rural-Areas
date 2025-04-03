;; local-verification.clar
;; Leverages community knowledge for validation

;; Store identity information locally to avoid contract dependencies
(define-map identities
  { id: uint }
  {
    owner: principal,
    active: bool
  }
)

(define-map verifications
  { identity-id: uint }
  {
    verifiers: (list 10 principal),
    verification-count: uint,
    verified: bool
  }
)

(define-constant MIN_VERIFICATIONS u3)

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

(define-public (verify-identity (identity-id uint))
  (let
    (
      (identity (map-get? identities { id: identity-id }))
      (verification (default-to
        { verifiers: (list), verification-count: u0, verified: false }
        (map-get? verifications { identity-id: identity-id })))
      (verifiers (get verifiers verification))
      (verification-count (get verification-count verification))
    )
    (asserts! (is-some identity) (err u1)) ;; Identity not found
    (asserts! (not (is-eq tx-sender (get owner (unwrap-panic identity)))) (err u2)) ;; Owner cannot verify themselves

    ;; Check if already verified by this verifier
    (asserts! (is-eq (index-of verifiers tx-sender) none) (err u3)) ;; Already verified

    (let
      (
        (new-verifiers (unwrap-panic (as-max-len? (append verifiers tx-sender) u10)))
        (new-count (+ verification-count u1))
        (is-verified (>= new-count MIN_VERIFICATIONS))
      )
      (map-set verifications
        { identity-id: identity-id }
        {
          verifiers: new-verifiers,
          verification-count: new-count,
          verified: is-verified
        }
      )
      (ok is-verified)
    )
  )
)

(define-read-only (get-verification-status (identity-id uint))
  (default-to
    { verifiers: (list), verification-count: u0, verified: false }
    (map-get? verifications { identity-id: identity-id })
  )
)

(define-read-only (is-identity-verified (identity-id uint))
  (get verified (get-verification-status identity-id))
)

(define-read-only (get-verification-count (identity-id uint))
  (get verification-count (get-verification-status identity-id))
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
