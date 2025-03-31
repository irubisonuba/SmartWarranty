;; SmartWarranty - Product warranty management system
;; Core features: warranty creation, transfer, claims, and maintenance tracking

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u100))
(define-constant err-warranty-not-found (err u101))
(define-constant err-expired-warranty (err u102))
(define-constant err-invalid-claim (err u103))
(define-constant err-maintenance-required (err u104))

;; Data variables
(define-data-var next-warranty-id uint u1)

;; Data maps
(define-map warranties
    uint 
    {
        product-id: (string-ascii 24),
        manufacturer: principal,
        owner: principal,
        issue-date: uint,
        expiry-date: uint,
        maintenance-count: uint,
        is-active: bool
    }
)

(define-map warranty-claims
    uint 
    {
        claim-date: uint,
        description: (string-ascii 256),
        status: (string-ascii 12)
    }
)

(define-map maintenance-records
    {warranty-id: uint, record-id: uint}
    {
        service-date: uint,
        description: (string-ascii 256),
        provider: principal
    }
)

;; Public functions

;; Create new warranty
(define-public (create-warranty (product-id (string-ascii 24)) (owner principal) (duration uint))
    (let
        (
            (warranty-id (var-get next-warranty-id))
            (issue-date stacks-block-height)
            (expiry-date (+ stacks-block-height duration))
        )
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (map-set warranties warranty-id
            {
                product-id: product-id,
                manufacturer: tx-sender,
                owner: owner,
                issue-date: issue-date,
                expiry-date: expiry-date,
                maintenance-count: u0,
                is-active: true
            }
        )
        (var-set next-warranty-id (+ warranty-id u1))
        (ok warranty-id)
    )
)

;; Transfer warranty to new owner
(define-public (transfer-warranty (warranty-id uint) (new-owner principal))
    (let
        (
            (warranty (unwrap! (map-get? warranties warranty-id) err-warranty-not-found))
        )
        (asserts! (is-eq (get owner warranty) tx-sender) err-not-authorized)
        (asserts! (get is-active warranty) err-expired-warranty)
        (map-set warranties warranty-id
            (merge warranty {owner: new-owner})
        )
        (ok true)
    )
)

;; File warranty claim
(define-public (file-claim (warranty-id uint) (description (string-ascii 256)))
    (let
        (
            (warranty (unwrap! (map-get? warranties warranty-id) err-warranty-not-found))
        )
        (asserts! (is-eq (get owner warranty) tx-sender) err-not-authorized)
        (asserts! (get is-active warranty) err-expired-warranty)
        (asserts! (<= stacks-block-height (get expiry-date warranty)) err-expired-warranty)
        (map-set warranty-claims warranty-id
            {
                claim-date: stacks-block-height,
                description: description,
                status: "PENDING"
            }
        )
        (ok true)
    )
)

;; Record maintenance
(define-public (record-maintenance (warranty-id uint) (description (string-ascii 256)))
    (let
        (
            (warranty (unwrap! (map-get? warranties warranty-id) err-warranty-not-found))
            (maintenance-id (get maintenance-count warranty))
        )
        (asserts! (get is-active warranty) err-expired-warranty)
        (map-set maintenance-records 
            {warranty-id: warranty-id, record-id: maintenance-id}
            {
                service-date: stacks-block-height,
                description: description,
                provider: tx-sender
            }
        )
        (map-set warranties warranty-id
            (merge warranty {maintenance-count: (+ maintenance-id u1)})
        )
        (ok true)
    )
)

;; Read-only functions

;; Get warranty details
(define-read-only (get-warranty (warranty-id uint))
    (ok (unwrap! (map-get? warranties warranty-id) err-warranty-not-found))
)

;; Get warranty claim
(define-read-only (get-warranty-claim (warranty-id uint))
    (ok (unwrap! (map-get? warranty-claims warranty-id) err-warranty-not-found))
)

;; Get maintenance record
(define-read-only (get-maintenance-record (warranty-id uint) (record-id uint))
    (ok (unwrap! 
        (map-get? maintenance-records {warranty-id: warranty-id, record-id: record-id})
        err-warranty-not-found))
)

;; Check if warranty is valid
(define-read-only (is-warranty-valid (warranty-id uint))
    (let
        (
            (warranty (unwrap! (map-get? warranties warranty-id) err-warranty-not-found))
        )
        (ok (and 
            (get is-active warranty)
            (<= stacks-block-height (get expiry-date warranty))
        ))
    )
)

