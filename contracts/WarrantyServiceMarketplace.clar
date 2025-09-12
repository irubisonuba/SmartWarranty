;; WarrantyServiceMarketplace - Decentralized marketplace for warranty-related services
;; Allows service providers to list services and warranty owners to book them

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u300))
(define-constant err-service-not-found (err u301))
(define-constant err-booking-not-found (err u302))
(define-constant err-insufficient-funds (err u303))
(define-constant err-invalid-rating (err u304))
(define-constant err-service-unavailable (err u305))
(define-constant err-booking-completed (err u306))
(define-constant err-invalid-parameters (err u307))

;; Data variables
(define-data-var next-service-id uint u1)
(define-data-var next-booking-id uint u1)
(define-data-var marketplace-fee-rate uint u5) ;; 5% marketplace fee

;; Service provider registry
(define-map service-providers
    principal
    {
        provider-name: (string-ascii 32),
        specialization: (string-ascii 24),
        certification-level: uint,
        is-verified: bool,
        service-radius: uint,
        total-services: uint,
        average-rating: uint,
        registration-date: uint
    }
)

;; Service listings
(define-map service-listings
    uint
    {
        provider: principal,
        service-name: (string-ascii 48),
        service-type: (string-ascii 24),
        description: (string-ascii 256),
        price: uint,
        estimated-duration: uint,
        warranty-types: (list 5 (string-ascii 24)),
        is-available: bool,
        created-at: uint
    }
)

;; Service bookings
(define-map service-bookings
    uint
    {
        service-id: uint,
        customer: principal,
        warranty-id: uint,
        scheduled-date: uint,
        payment-amount: uint,
        booking-status: (string-ascii 16),
        completion-date: (optional uint),
        customer-rating: (optional uint),
        provider-notes: (optional (string-ascii 256))
    }
)

;; Service availability calendar
(define-map provider-availability
    {provider: principal, date: uint}
    {
        available-slots: uint,
        booked-slots: uint,
        hourly-rate: uint
    }
)

;; Public functions

;; Register as service provider
(define-public (register-service-provider 
    (provider-name (string-ascii 32))
    (specialization (string-ascii 24))
    (service-radius uint))
    (begin
        (map-set service-providers tx-sender
            {
                provider-name: provider-name,
                specialization: specialization,
                certification-level: u1,
                is-verified: false,
                service-radius: service-radius,
                total-services: u0,
                average-rating: u0,
                registration-date: stacks-block-height
            }
        )
        (ok true)
    )
)

;; Verify service provider (admin only)
(define-public (verify-service-provider (provider principal) (certification-level uint))
    (let
        (
            (provider-data (unwrap! (map-get? service-providers provider) err-not-authorized))
        )
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (asserts! (<= certification-level u5) err-invalid-parameters)
        (map-set service-providers provider
            (merge provider-data {
                is-verified: true,
                certification-level: certification-level
            })
        )
        (ok true)
    )
)

;; Create service listing
(define-public (create-service-listing
    (service-name (string-ascii 48))
    (service-type (string-ascii 24))
    (description (string-ascii 256))
    (price uint)
    (estimated-duration uint)
    (warranty-types (list 5 (string-ascii 24))))
    (let
        (
            (service-id (var-get next-service-id))
            (provider-data (unwrap! (map-get? service-providers tx-sender) err-not-authorized))
        )
        (asserts! (get is-verified provider-data) err-not-authorized)
        (asserts! (> price u0) err-invalid-parameters)
        (asserts! (> estimated-duration u0) err-invalid-parameters)
        
        (map-set service-listings service-id
            {
                provider: tx-sender,
                service-name: service-name,
                service-type: service-type,
                description: description,
                price: price,
                estimated-duration: estimated-duration,
                warranty-types: warranty-types,
                is-available: true,
                created-at: stacks-block-height
            }
        )
        (var-set next-service-id (+ service-id u1))
        (ok service-id)
    )
)

;; Book a service
(define-public (book-service 
    (service-id uint)
    (warranty-id uint)
    (scheduled-date uint))
    (let
        (
            (service (unwrap! (map-get? service-listings service-id) err-service-not-found))
            (booking-id (var-get next-booking-id))
            (service-price (get price service))
            (marketplace-fee (/ (* service-price (var-get marketplace-fee-rate)) u100))
            (provider-payment (- service-price marketplace-fee))
        )
        (asserts! (get is-available service) err-service-unavailable)
        (asserts! (>= (stx-get-balance tx-sender) service-price) err-insufficient-funds)
        (asserts! (> scheduled-date stacks-block-height) err-invalid-parameters)
        
        ;; Transfer payment
        (try! (stx-transfer? provider-payment tx-sender (get provider service)))
        (try! (stx-transfer? marketplace-fee tx-sender contract-owner))
        
        ;; Create booking record
        (map-set service-bookings booking-id
            {
                service-id: service-id,
                customer: tx-sender,
                warranty-id: warranty-id,
                scheduled-date: scheduled-date,
                payment-amount: service-price,
                booking-status: "BOOKED",
                completion-date: none,
                customer-rating: none,
                provider-notes: none
            }
        )
        
        (var-set next-booking-id (+ booking-id u1))
        (ok booking-id)
    )
)

;; Complete service and update booking
(define-public (complete-service 
    (booking-id uint)
    (provider-notes (string-ascii 256)))
    (let
        (
            (booking (unwrap! (map-get? service-bookings booking-id) err-booking-not-found))
            (service (unwrap! (map-get? service-listings (get service-id booking)) err-service-not-found))
        )
        (asserts! (is-eq tx-sender (get provider service)) err-not-authorized)
        (asserts! (is-eq (get booking-status booking) "BOOKED") err-booking-completed)
        
        (map-set service-bookings booking-id
            (merge booking {
                booking-status: "COMPLETED",
                completion-date: (some stacks-block-height),
                provider-notes: (some provider-notes)
            })
        )
        
        ;; Update provider statistics
        (let
            (
                (provider-data (unwrap! (map-get? service-providers tx-sender) err-not-authorized))
            )
            (map-set service-providers tx-sender
                (merge provider-data {
                    total-services: (+ (get total-services provider-data) u1)
                })
            )
        )
        (ok true)
    )
)

;; Rate completed service
(define-public (rate-service (booking-id uint) (rating uint))
    (let
        (
            (booking (unwrap! (map-get? service-bookings booking-id) err-booking-not-found))
            (service (unwrap! (map-get? service-listings (get service-id booking)) err-service-not-found))
        )
        (asserts! (is-eq tx-sender (get customer booking)) err-not-authorized)
        (asserts! (is-eq (get booking-status booking) "COMPLETED") err-booking-not-found)
        (asserts! (<= rating u5) err-invalid-rating)
        (asserts! (> rating u0) err-invalid-rating)
        
        (map-set service-bookings booking-id
            (merge booking {customer-rating: (some rating)})
        )
        
        ;; Update provider average rating
        (let
            (
                (provider-data (unwrap! (map-get? service-providers (get provider service)) err-not-authorized))
                (current-rating (get average-rating provider-data))
                (total-services (get total-services provider-data))
                (new-average (if (is-eq total-services u0) 
                    rating 
                    (/ (+ (* current-rating total-services) rating) (+ total-services u1))))
            )
            (map-set service-providers (get provider service)
                (merge provider-data {average-rating: new-average})
            )
        )
        (ok true)
    )
)

;; Set service availability
(define-public (set-availability 
    (date uint)
    (available-slots uint)
    (hourly-rate uint))
    (let
        (
            (provider-data (unwrap! (map-get? service-providers tx-sender) err-not-authorized))
        )
        (asserts! (get is-verified provider-data) err-not-authorized)
        (asserts! (> available-slots u0) err-invalid-parameters)
        (asserts! (> hourly-rate u0) err-invalid-parameters)
        
        (map-set provider-availability {provider: tx-sender, date: date}
            {
                available-slots: available-slots,
                booked-slots: u0,
                hourly-rate: hourly-rate
            }
        )
        (ok true)
    )
)

;; Update service listing availability
(define-public (toggle-service-availability (service-id uint) (available bool))
    (let
        (
            (service (unwrap! (map-get? service-listings service-id) err-service-not-found))
        )
        (asserts! (is-eq tx-sender (get provider service)) err-not-authorized)
        (map-set service-listings service-id
            (merge service {is-available: available})
        )
        (ok true)
    )
)

;; Read-only functions

;; Get service provider details
(define-read-only (get-service-provider (provider principal))
    (ok (map-get? service-providers provider))
)

;; Get service listing details
(define-read-only (get-service-listing (service-id uint))
    (ok (map-get? service-listings service-id))
)

;; Get booking details
(define-read-only (get-service-booking (booking-id uint))
    (ok (map-get? service-bookings booking-id))
)

;; Get provider availability
(define-read-only (get-provider-availability (provider principal) (date uint))
    (ok (map-get? provider-availability {provider: provider, date: date}))
)

;; Get marketplace fee rate
(define-read-only (get-marketplace-fee-rate)
    (ok (var-get marketplace-fee-rate))
)

;; Check if provider is verified
(define-read-only (is-provider-verified (provider principal))
    (let
        (
            (provider-data (map-get? service-providers provider))
        )
        (match provider-data
            some-data (ok (get is-verified some-data))
            (ok false)
        )
    )
)

;; Get service cost with fees
(define-read-only (calculate-total-cost (service-id uint))
    (let
        (
            (service (unwrap! (map-get? service-listings service-id) err-service-not-found))
            (base-price (get price service))
            (marketplace-fee (/ (* base-price (var-get marketplace-fee-rate)) u100))
        )
        (ok {
            base-price: base-price,
            marketplace-fee: marketplace-fee,
            total-cost: base-price
        })
    )
)

;; Administrative functions

;; Update marketplace fee (admin only)
(define-public (update-marketplace-fee (new-fee-rate uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (asserts! (<= new-fee-rate u20) err-invalid-parameters) ;; Max 20% fee
        (var-set marketplace-fee-rate new-fee-rate)
        (ok true)
    )
)
