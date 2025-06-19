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


(define-public (extend-warranty (warranty-id uint) (extension-duration uint))
    (let
        (
            (warranty (unwrap! (map-get? warranties warranty-id) err-warranty-not-found))
            (new-expiry (+ (get expiry-date warranty) extension-duration))
        )
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (asserts! (get is-active warranty) err-expired-warranty)
        (map-set warranties warranty-id
            (merge warranty {expiry-date: new-expiry})
        )
        (ok true)
    )
)



(define-public (resolve-claim (warranty-id uint) (resolution (string-ascii 12)))
    (let
        (
            (claim (unwrap! (map-get? warranty-claims warranty-id) err-warranty-not-found))
        )
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (map-set warranty-claims warranty-id
            (merge claim {status: resolution})
        )
        (ok true)
    )
)


(define-constant err-invalid-status (err u105))
(define-public (update-warranty-status (warranty-id uint) (new-status bool))
    (let
        (
            (warranty (unwrap! (map-get? warranties warranty-id) err-warranty-not-found))
        )
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (map-set warranties warranty-id
            (merge warranty {is-active: new-status})
        )
        (ok true)
    )
)

(define-map warranty-ratings
    uint 
    {
        rating: uint,
        review: (string-ascii 256),
        reviewer: principal
    }
)

(define-public (rate-warranty (warranty-id uint) (rating uint) (review (string-ascii 256)))
    (let
        (
            (warranty (unwrap! (map-get? warranties warranty-id) err-warranty-not-found))
        )
        (asserts! (is-eq (get owner warranty) tx-sender) err-not-authorized)
        (asserts! (<= rating u5) (err u106))
        (map-set warranty-ratings warranty-id
            {
                rating: rating,
                review: review,
                reviewer: tx-sender
            }
        )
        (ok true)
    )
)


(define-map maintenance-schedule
    uint
    {
        last-maintenance: uint,
        maintenance-interval: uint,
        next-due: uint
    }
)

(define-public (set-maintenance-schedule (warranty-id uint) (interval uint))
    (let
        (
            (warranty (unwrap! (map-get? warranties warranty-id) err-warranty-not-found))
            (current-block stacks-block-height)
        )
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (map-set maintenance-schedule warranty-id
            {
                last-maintenance: current-block,
                maintenance-interval: interval,
                next-due: (+ current-block interval)
            }
        )
        (ok true)
    )
)

(define-map warranty-history
    {warranty-id: uint, event-id: uint}
    {
        event-type: (string-ascii 24),
        event-date: uint,
        description: (string-ascii 256)
    }
)

(define-data-var next-event-id uint u1)

(define-public (add-warranty-event (warranty-id uint) (event-type (string-ascii 24)) (description (string-ascii 256)))
    (let
        (
            (warranty (unwrap! (map-get? warranties warranty-id) err-warranty-not-found))
            (event-id (var-get next-event-id))
        )
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (map-set warranty-history
            {warranty-id: warranty-id, event-id: event-id}
            {
                event-type: event-type,
                event-date: stacks-block-height,
                description: description
            }
        )
        (var-set next-event-id (+ event-id u1))
        (ok event-id)
    )
)

(define-map transfer-history
    {warranty-id: uint, transfer-id: uint}
    {
        from: principal,
        to: principal,
        transfer-date: uint
    }
)

(define-data-var next-transfer-id uint u1)

(define-public (record-transfer (warranty-id uint) (from principal) (to principal))
    (let
        (
            (transfer-id (var-get next-transfer-id))
        )
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (map-set transfer-history
            {warranty-id: warranty-id, transfer-id: transfer-id}
            {
                from: from,
                to: to,
                transfer-date: stacks-block-height
            }
        )
        (var-set next-transfer-id (+ transfer-id u1))
        (ok transfer-id)
    )
)


(define-map warranty-bundles
    uint
    {
        bundle-name: (string-ascii 24),
        warranties: (list 10 uint),
        discount-rate: uint,
        created-at: uint,
        owner: principal
    }
)

(define-data-var next-bundle-id uint u1)

(define-public (create-warranty-bundle 
    (bundle-name (string-ascii 24))
    (warranty-ids (list 10 uint))
    (discount-rate uint))
    (let
        ((bundle-id (var-get next-bundle-id)))
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (map-set warranty-bundles bundle-id
            {
                bundle-name: bundle-name,
                warranties: warranty-ids,
                discount-rate: discount-rate,
                created-at: stacks-block-height,
                owner: tx-sender
            }
        )
        (var-set next-bundle-id (+ bundle-id u1))
        (ok bundle-id)
    )
)

(define-read-only (get-warranty-bundle (bundle-id uint))
    (ok (unwrap! (map-get? warranty-bundles bundle-id) err-warranty-not-found))
)


(define-public (transfer-bundle (bundle-id uint) (new-owner principal))
    (let
        (
            (bundle (unwrap! (map-get? warranty-bundles bundle-id) err-warranty-not-found))
        )
        (asserts! (is-eq (get owner bundle) tx-sender) err-not-authorized)
        (map-set warranty-bundles bundle-id
            (merge bundle {owner: new-owner})
        )
        (ok true)
    )
)
(define-public (get-bundle-warranties (bundle-id uint))
    (let
        (
            (bundle (unwrap! (map-get? warranty-bundles bundle-id) err-warranty-not-found))
        )
        (ok (get warranties bundle))
    )
)
(define-public (get-bundle-discount (bundle-id uint))
    (let
        (
            (bundle (unwrap! (map-get? warranty-bundles bundle-id) err-warranty-not-found))
        )
        (ok (get discount-rate bundle))
    )
)
(define-public (get-bundle-owner (bundle-id uint))
    (let
        (
            (bundle (unwrap! (map-get? warranty-bundles bundle-id) err-warranty-not-found))
        )
        (ok (get owner bundle))
    )
)
(define-public (get-bundle-creation-date (bundle-id uint))
    (let
        (
            (bundle (unwrap! (map-get? warranty-bundles bundle-id) err-warranty-not-found))
        )
        (ok (get created-at bundle))
    )
)
(define-public (get-bundle-warranty-count (bundle-id uint))
    (let
        (
            (bundle (unwrap! (map-get? warranty-bundles bundle-id) err-warranty-not-found))
        )
        (ok (len (get warranties bundle)))
    )
)


(define-non-fungible-token warranty-certificate uint)

(define-map certificate-metadata
    uint
    {
        warranty-id: uint,
        metadata-uri: (string-utf8 256),
        issued-date: uint
    }
)

(define-public (mint-warranty-certificate 
    (warranty-id uint) 
    (metadata-uri (string-utf8 256)))
    (let
        ((warranty (unwrap! (map-get? warranties warranty-id) err-warranty-not-found)))
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (try! (nft-mint? warranty-certificate warranty-id (get owner warranty)))
        (map-set certificate-metadata warranty-id
            {
                warranty-id: warranty-id,
                metadata-uri: metadata-uri,
                issued-date: stacks-block-height
            }
        )
        (ok true)
    )
)

(define-read-only (get-certificate-metadata (certificate-id uint))
    (ok (unwrap! (map-get? certificate-metadata certificate-id) err-warranty-not-found))
)

(define-constant err-insufficient-funds (err u107))
(define-constant err-insurance-not-found (err u108))
(define-constant err-insurance-expired (err u109))
(define-constant err-claim-already-processed (err u110))

(define-data-var next-insurance-id uint u1)
(define-data-var insurance-pool uint u0)

(define-map insurance-policies
    uint
    {
        warranty-id: uint,
        policy-holder: principal,
        premium-paid: uint,
        coverage-amount: uint,
        start-date: uint,
        end-date: uint,
        is-active: bool
    }
)

(define-map insurance-claims
    uint
    {
        insurance-id: uint,
        claim-amount: uint,
        claim-date: uint,
        status: (string-ascii 12),
        processed-date: (optional uint)
    }
)

(define-map warranty-insurance-link
    uint
    uint
)

(define-public (purchase-insurance 
    (warranty-id uint) 
    (coverage-amount uint) 
    (duration uint))
    (let
        (
            (warranty (unwrap! (map-get? warranties warranty-id) err-warranty-not-found))
            (insurance-id (var-get next-insurance-id))
            (premium (calculate-premium coverage-amount duration))
        )
        (asserts! (is-eq (get owner warranty) tx-sender) err-not-authorized)
        (asserts! (get is-active warranty) err-expired-warranty)
        (asserts! (>= (stx-get-balance tx-sender) premium) err-insufficient-funds)
        
        (try! (stx-transfer? premium tx-sender (as-contract tx-sender)))
        (var-set insurance-pool (+ (var-get insurance-pool) premium))
        
        (map-set insurance-policies insurance-id
            {
                warranty-id: warranty-id,
                policy-holder: tx-sender,
                premium-paid: premium,
                coverage-amount: coverage-amount,
                start-date: stacks-block-height,
                end-date: (+ stacks-block-height duration),
                is-active: true
            }
        )
        
        (map-set warranty-insurance-link warranty-id insurance-id)
        (var-set next-insurance-id (+ insurance-id u1))
        (ok insurance-id)
    )
)

(define-public (file-insurance-claim 
    (insurance-id uint) 
    (claim-amount uint))
    (let
        (
            (policy (unwrap! (map-get? insurance-policies insurance-id) err-insurance-not-found))
        )
        (asserts! (is-eq (get policy-holder policy) tx-sender) err-not-authorized)
        (asserts! (get is-active policy) err-insurance-expired)
        (asserts! (<= stacks-block-height (get end-date policy)) err-insurance-expired)
        (asserts! (<= claim-amount (get coverage-amount policy)) err-invalid-claim)
        
        (map-set insurance-claims insurance-id
            {
                insurance-id: insurance-id,
                claim-amount: claim-amount,
                claim-date: stacks-block-height,
                status: "PENDING",
                processed-date: none
            }
        )
        (ok true)
    )
)

(define-public (process-insurance-claim 
    (insurance-id uint) 
    (approved bool))
    (let
        (
            (claim (unwrap! (map-get? insurance-claims insurance-id) err-insurance-not-found))
            (policy (unwrap! (map-get? insurance-policies insurance-id) err-insurance-not-found))
            (claim-amount (get claim-amount claim))
        )
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (asserts! (is-eq (get status claim) "PENDING") err-claim-already-processed)
        
        (if approved
            (begin
                (asserts! (>= (var-get insurance-pool) claim-amount) err-insufficient-funds)
                (try! (as-contract (stx-transfer? claim-amount tx-sender (get policy-holder policy))))
                (var-set insurance-pool (- (var-get insurance-pool) claim-amount))
                (map-set insurance-claims insurance-id
                    (merge claim {
                        status: "APPROVED",
                        processed-date: (some stacks-block-height)
                    })
                )
            )
            (map-set insurance-claims insurance-id
                (merge claim {
                    status: "REJECTED",
                    processed-date: (some stacks-block-height)
                })
            )
        )
        (ok true)
    )
)

(define-public (cancel-insurance-policy (insurance-id uint))
    (let
        (
            (policy (unwrap! (map-get? insurance-policies insurance-id) err-insurance-not-found))
            (refund-amount (/ (get premium-paid policy) u2))
        )
        (asserts! (is-eq (get policy-holder policy) tx-sender) err-not-authorized)
        (asserts! (get is-active policy) err-insurance-expired)
        
        (try! (as-contract (stx-transfer? refund-amount tx-sender (get policy-holder policy))))
        (var-set insurance-pool (- (var-get insurance-pool) refund-amount))
        
        (map-set insurance-policies insurance-id
            (merge policy {is-active: false})
        )
        (ok refund-amount)
    )
)

(define-read-only (calculate-premium (coverage-amount uint) (duration uint))
    (let
        (
            (base-rate u10)
            (duration-factor (/ duration u1000))
            (coverage-factor (/ coverage-amount u100))
        )
        (+ base-rate (* duration-factor coverage-factor))
    )
)

(define-read-only (get-insurance-policy (insurance-id uint))
    (ok (unwrap! (map-get? insurance-policies insurance-id) err-insurance-not-found))
)

(define-read-only (get-insurance-claim (insurance-id uint))
    (ok (unwrap! (map-get? insurance-claims insurance-id) err-insurance-not-found))
)

(define-read-only (get-warranty-insurance (warranty-id uint))
    (ok (map-get? warranty-insurance-link warranty-id))
)

(define-read-only (get-insurance-pool-balance)
    (ok (var-get insurance-pool))
)

(define-read-only (is-insurance-active (insurance-id uint))
    (let
        (
            (policy (unwrap! (map-get? insurance-policies insurance-id) err-insurance-not-found))
        )
        (ok (and 
            (get is-active policy)
            (<= stacks-block-height (get end-date policy))
        ))
    )
)