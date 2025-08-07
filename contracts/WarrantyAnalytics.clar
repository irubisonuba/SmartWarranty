;; WarrantyAnalytics - Performance analytics and reputation system for warranty management
;; Tracks detailed performance metrics, generates reputation scores, and provides data-driven insights

;; Main SmartWarranty contract integration for analytics

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u200))
(define-constant err-invalid-parameters (err u201))
(define-constant err-analytics-not-found (err u202))
(define-constant err-reputation-not-found (err u203))
(define-constant err-insufficient-data (err u204))

;; Data variables for tracking analytics
(define-data-var total-warranties-analyzed uint u0)
(define-data-var total-claims-processed uint u0)
(define-data-var analytics-enabled bool true)

;; Performance metrics for manufacturers
(define-map manufacturer-performance
    principal
    {
        total-warranties-issued: uint,
        active-warranties: uint,
        total-claims-filed: uint,
        claims-approved: uint,
        claims-rejected: uint,
        average-warranty-duration: uint,
        maintenance-frequency: uint,
        reputation-score: uint,
        last-updated: uint
    }
)

;; Performance metrics for service providers
(define-map service-provider-performance
    principal
    {
        total-services-provided: uint,
        warranty-extensions-granted: uint,
        customer-satisfaction-score: uint,
        response-time-average: uint,
        service-quality-rating: uint,
        reputation-score: uint,
        certification-level: uint,
        last-updated: uint
    }
)

;; Warranty lifecycle analytics
(define-map warranty-lifecycle-data
    uint
    {
        warranty-id: uint,
        creation-date: uint,
        first-claim-date: (optional uint),
        total-maintenance-events: uint,
        owner-changes: uint,
        total-rating-events: uint,
        performance-score: uint,
        risk-assessment: uint,
        predicted-lifespan: uint
    }
)

;; Product category performance tracking
(define-map product-category-metrics
    (string-ascii 24)
    {
        category-name: (string-ascii 24),
        total-warranties: uint,
        average-claim-rate: uint,
        most-common-failure: (string-ascii 128),
        average-satisfaction: uint,
        maintenance-pattern: uint,
        seasonal-trends: uint,
        replacement-rate: uint
    }
)

;; Time-based performance analytics
(define-map time-period-analytics
    {period-type: (string-ascii 12), period-value: uint}
    {
        warranties-created: uint,
        claims-filed: uint,
        claims-resolved: uint,
        maintenance-performed: uint,
        transfers-completed: uint,
        revenue-generated: uint,
        customer-acquisition: uint
    }
)

;; Risk assessment data for predictive analytics
(define-map risk-assessment-data
    uint
    {
        warranty-id: uint,
        failure-probability: uint,
        maintenance-urgency: uint,
        replacement-likelihood: uint,
        insurance-risk-level: uint,
        environmental-factors: uint,
        usage-pattern-score: uint,
        assessment-date: uint
    }
)

;; Customer behavior analytics
(define-map customer-behavior-metrics
    principal
    {
        total-warranties-owned: uint,
        average-ownership-duration: uint,
        claim-frequency: uint,
        maintenance-adherence: uint,
        transfer-activity: uint,
        satisfaction-trend: uint,
        loyalty-score: uint,
        risk-profile: uint
    }
)

;; Comparative market analytics
(define-map market-comparison-data
    (string-ascii 24)
    {
        product-category: (string-ascii 24),
        market-share: uint,
        competitive-advantage: uint,
        price-performance-ratio: uint,
        innovation-score: uint,
        market-trend: uint,
        growth-potential: uint,
        benchmark-position: uint
    }
)

;; Public functions for updating analytics data

;; Record warranty creation analytics
(define-public (record-warranty-creation 
    (warranty-id uint) 
    (manufacturer principal) 
    (product-category (string-ascii 24))
    (estimated-lifespan uint))
    (let
        (
            (current-manufacturer-data (default-to 
                {
                    total-warranties-issued: u0,
                    active-warranties: u0,
                    total-claims-filed: u0,
                    claims-approved: u0,
                    claims-rejected: u0,
                    average-warranty-duration: u0,
                    maintenance-frequency: u0,
                    reputation-score: u50,
                    last-updated: u0
                }
                (map-get? manufacturer-performance manufacturer)
            ))
            (current-category-data (default-to
                {
                    category-name: product-category,
                    total-warranties: u0,
                    average-claim-rate: u0,
                    most-common-failure: "",
                    average-satisfaction: u0,
                    maintenance-pattern: u0,
                    seasonal-trends: u0,
                    replacement-rate: u0
                }
                (map-get? product-category-metrics product-category)
            ))
        )
        (asserts! (var-get analytics-enabled) err-not-authorized)
        
        ;; Update manufacturer performance
        (map-set manufacturer-performance manufacturer
            (merge current-manufacturer-data {
                total-warranties-issued: (+ (get total-warranties-issued current-manufacturer-data) u1),
                active-warranties: (+ (get active-warranties current-manufacturer-data) u1),
                last-updated: stacks-block-height
            })
        )
        
        ;; Update product category metrics
        (map-set product-category-metrics product-category
            (merge current-category-data {
                total-warranties: (+ (get total-warranties current-category-data) u1)
            })
        )
        
        ;; Initialize warranty lifecycle data
        (map-set warranty-lifecycle-data warranty-id
            {
                warranty-id: warranty-id,
                creation-date: stacks-block-height,
                first-claim-date: none,
                total-maintenance-events: u0,
                owner-changes: u0,
                total-rating-events: u0,
                performance-score: u100,
                risk-assessment: u25,
                predicted-lifespan: estimated-lifespan
            }
        )
        
        ;; Update global counters
        (var-set total-warranties-analyzed (+ (var-get total-warranties-analyzed) u1))
        (ok true)
    )
)

;; Record claim analytics
(define-public (record-claim-analytics 
    (warranty-id uint) 
    (manufacturer principal) 
    (claim-type (string-ascii 24))
    (resolution-time uint))
    (let
        (
            (manufacturer-data (unwrap! (map-get? manufacturer-performance manufacturer) err-analytics-not-found))
            (lifecycle-data (unwrap! (map-get? warranty-lifecycle-data warranty-id) err-analytics-not-found))
        )
        (asserts! (var-get analytics-enabled) err-not-authorized)
        
        ;; Update manufacturer claim statistics
        (map-set manufacturer-performance manufacturer
            (merge manufacturer-data {
                total-claims-filed: (+ (get total-claims-filed manufacturer-data) u1),
                last-updated: stacks-block-height
            })
        )
        
        ;; Update warranty lifecycle data
        (map-set warranty-lifecycle-data warranty-id
            (merge lifecycle-data {
                first-claim-date: (if (is-none (get first-claim-date lifecycle-data))
                    (some stacks-block-height)
                    (get first-claim-date lifecycle-data)
                ),
                performance-score: (if (> (get performance-score lifecycle-data) u10)
                    (- (get performance-score lifecycle-data) u10)
                    u0
                )
            })
        )
        
        (var-set total-claims-processed (+ (var-get total-claims-processed) u1))
        (ok true)
    )
)

;; Record maintenance analytics
(define-public (record-maintenance-analytics 
    (warranty-id uint) 
    (service-provider principal) 
    (service-quality uint)
    (response-time uint))
    (let
        (
            (provider-data (default-to
                {
                    total-services-provided: u0,
                    warranty-extensions-granted: u0,
                    customer-satisfaction-score: u0,
                    response-time-average: u0,
                    service-quality-rating: u0,
                    reputation-score: u50,
                    certification-level: u1,
                    last-updated: u0
                }
                (map-get? service-provider-performance service-provider)
            ))
            (lifecycle-data (unwrap! (map-get? warranty-lifecycle-data warranty-id) err-analytics-not-found))
        )
        (asserts! (var-get analytics-enabled) err-not-authorized)
        (asserts! (<= service-quality u100) err-invalid-parameters)
        
        ;; Update service provider performance
        (map-set service-provider-performance service-provider
            (merge provider-data {
                total-services-provided: (+ (get total-services-provided provider-data) u1),
                response-time-average: (/ (+ (* (get response-time-average provider-data) 
                    (get total-services-provided provider-data)) response-time) 
                    (+ (get total-services-provided provider-data) u1)),
                service-quality-rating: (/ (+ (* (get service-quality-rating provider-data) 
                    (get total-services-provided provider-data)) service-quality) 
                    (+ (get total-services-provided provider-data) u1)),
                last-updated: stacks-block-height
            })
        )
        
        ;; Update warranty maintenance events
        (map-set warranty-lifecycle-data warranty-id
            (merge lifecycle-data {
                total-maintenance-events: (+ (get total-maintenance-events lifecycle-data) u1),
                performance-score: (if (< (get performance-score lifecycle-data) u90)
                    (+ (get performance-score lifecycle-data) u5)
                    u100
                )
            })
        )
        
        (ok true)
    )
)

;; Calculate and update reputation scores
(define-public (calculate-reputation-scores (entity principal) (entity-type (string-ascii 12)))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (if (is-eq entity-type "manufacturer")
            (let
                (
                    (perf-data (unwrap! (map-get? manufacturer-performance entity) err-reputation-not-found))
                    (total-warranties (get total-warranties-issued perf-data))
                    (total-claims (get total-claims-filed perf-data))
                    (approved-claims (get claims-approved perf-data))
                )
                (asserts! (> total-warranties u0) err-insufficient-data)
                (let
                    (
                        (claim-rate (if (> total-warranties u0) (/ (* total-claims u100) total-warranties) u0))
                        (approval-rate (if (> total-claims u0) (/ (* approved-claims u100) total-claims) u0))
                        (reputation-score (+ (- u100 (if (<= claim-rate u50) claim-rate u50)) (/ approval-rate u2)))
                    )
                    (map-set manufacturer-performance entity
                        (merge perf-data {
                            reputation-score: (if (<= reputation-score u100) reputation-score u100),
                            last-updated: stacks-block-height
                        })
                    )
                    (ok reputation-score)
                )
            )
            (if (is-eq entity-type "provider")
                (let
                    (
                        (provider-data (unwrap! (map-get? service-provider-performance entity) err-reputation-not-found))
                        (services-provided (get total-services-provided provider-data))
                        (quality-rating (get service-quality-rating provider-data))
                        (satisfaction-score (get customer-satisfaction-score provider-data))
                    )
                    (asserts! (> services-provided u0) err-insufficient-data)
                    (let
                        (
                            (reputation-score (/ (+ quality-rating satisfaction-score) u2))
                        )
                        (map-set service-provider-performance entity
                            (merge provider-data {
                                reputation-score: reputation-score,
                                last-updated: stacks-block-height
                            })
                        )
                        (ok reputation-score)
                    )
                )
                err-invalid-parameters
            )
        )
    )
)

;; Generate risk assessment
(define-public (generate-risk-assessment 
    (warranty-id uint) 
    (usage-pattern uint) 
    (environmental-factors uint))
    (let
        (
            (lifecycle-data (unwrap! (map-get? warranty-lifecycle-data warranty-id) err-analytics-not-found))
            (maintenance-events (get total-maintenance-events lifecycle-data))
            (performance-score (get performance-score lifecycle-data))
        )
        (asserts! (var-get analytics-enabled) err-not-authorized)
        (asserts! (<= usage-pattern u100) err-invalid-parameters)
        (asserts! (<= environmental-factors u100) err-invalid-parameters)
        
        (let
            (
                (failure-probability (+ (/ (- u100 performance-score) u2) 
                    (/ usage-pattern u4) (/ environmental-factors u4)))
                (maintenance-urgency (if (> maintenance-events u3) u25 
                    (- u75 (* maintenance-events u15))))
                (replacement-likelihood (if (< performance-score u30) u80 
                    (/ (- u100 performance-score) u2)))
                (insurance-risk-level (/ (+ failure-probability replacement-likelihood) u2))
            )
            (map-set risk-assessment-data warranty-id
                {
                    warranty-id: warranty-id,
                    failure-probability: (if (<= failure-probability u100) failure-probability u100),
                    maintenance-urgency: (if (<= maintenance-urgency u100) maintenance-urgency u100),
                    replacement-likelihood: (if (<= replacement-likelihood u100) replacement-likelihood u100),
                    insurance-risk-level: (if (<= insurance-risk-level u100) insurance-risk-level u100),
                    environmental-factors: environmental-factors,
                    usage-pattern-score: usage-pattern,
                    assessment-date: stacks-block-height
                }
            )
            (ok insurance-risk-level)
        )
    )
)

;; Update customer behavior metrics
(define-public (update-customer-behavior 
    (customer principal) 
    (action-type (string-ascii 16)) 
    (satisfaction-rating uint))
    (let
        (
            (behavior-data (default-to
                {
                    total-warranties-owned: u0,
                    average-ownership-duration: u0,
                    claim-frequency: u0,
                    maintenance-adherence: u0,
                    transfer-activity: u0,
                    satisfaction-trend: u50,
                    loyalty-score: u50,
                    risk-profile: u25
                }
                (map-get? customer-behavior-metrics customer)
            ))
        )
        (asserts! (var-get analytics-enabled) err-not-authorized)
        (asserts! (<= satisfaction-rating u100) err-invalid-parameters)
        
        (if (is-eq action-type "purchase")
            (map-set customer-behavior-metrics customer
                (merge behavior-data {
                    total-warranties-owned: (+ (get total-warranties-owned behavior-data) u1),
                    loyalty-score: (if (< (get loyalty-score behavior-data) u95)
                        (+ (get loyalty-score behavior-data) u5)
                        u100
                    )
                })
            )
            (if (is-eq action-type "claim")
                (map-set customer-behavior-metrics customer
                    (merge behavior-data {
                        claim-frequency: (+ (get claim-frequency behavior-data) u1),
                        risk-profile: (if (< (get risk-profile behavior-data) u90)
                            (+ (get risk-profile behavior-data) u10)
                            u100
                        )
                    })
                )
                (if (is-eq action-type "maintenance")
                    (map-set customer-behavior-metrics customer
                        (merge behavior-data {
                            maintenance-adherence: (if (< (get maintenance-adherence behavior-data) u95)
                                (+ (get maintenance-adherence behavior-data) u5)
                                u100
                            )
                        })
                    )
                    (map-set customer-behavior-metrics customer
                        (merge behavior-data {
                            satisfaction-trend: (/ (+ (get satisfaction-trend behavior-data) satisfaction-rating) u2)
                        })
                    )
                )
            )
        )
        (ok true)
    )
)

;; Read-only functions for analytics retrieval

;; Get manufacturer performance data
(define-read-only (get-manufacturer-performance (manufacturer principal))
    (ok (map-get? manufacturer-performance manufacturer))
)

;; Get service provider performance data
(define-read-only (get-service-provider-performance (provider principal))
    (ok (map-get? service-provider-performance provider))
)

;; Get warranty lifecycle analytics
(define-read-only (get-warranty-lifecycle-data (warranty-id uint))
    (ok (map-get? warranty-lifecycle-data warranty-id))
)

;; Get product category metrics
(define-read-only (get-product-category-metrics (category (string-ascii 24)))
    (ok (map-get? product-category-metrics category))
)

;; Get risk assessment data
(define-read-only (get-risk-assessment (warranty-id uint))
    (ok (map-get? risk-assessment-data warranty-id))
)

;; Get customer behavior metrics
(define-read-only (get-customer-behavior (customer principal))
    (ok (map-get? customer-behavior-metrics customer))
)

;; Get comprehensive analytics summary
(define-read-only (get-analytics-summary)
    (ok {
        total-warranties-analyzed: (var-get total-warranties-analyzed),
        total-claims-processed: (var-get total-claims-processed),
        analytics-enabled: (var-get analytics-enabled)
    })
)

;; Get top performing manufacturers (simplified ranking)
(define-read-only (get-manufacturer-ranking (manufacturer principal))
    (let
        (
            (perf-data (map-get? manufacturer-performance manufacturer))
        )
        (match perf-data
            some-data (ok (get reputation-score some-data))
            (ok u0)
        )
    )
)

;; Get warranty performance prediction
(define-read-only (predict-warranty-performance (warranty-id uint))
    (let
        (
            (lifecycle-data (map-get? warranty-lifecycle-data warranty-id))
            (risk-data (map-get? risk-assessment-data warranty-id))
        )
        (match lifecycle-data
            some-lifecycle (match risk-data
                some-risk (ok {
                    current-performance: (get performance-score some-lifecycle),
                    predicted-lifespan: (get predicted-lifespan some-lifecycle),
                    failure-risk: (get failure-probability some-risk),
                    maintenance-priority: (get maintenance-urgency some-risk)
                })
                (ok {
                    current-performance: (get performance-score some-lifecycle),
                    predicted-lifespan: (get predicted-lifespan some-lifecycle),
                    failure-risk: u25,
                    maintenance-priority: u25
                })
            )
            (err err-analytics-not-found)
        )
    )
)

;; Administrative functions

;; Toggle analytics system
(define-public (toggle-analytics-system (enabled bool))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (var-set analytics-enabled enabled)
        (ok enabled)
    )
)

;; Reset analytics data (emergency function)
(define-public (reset-analytics-counters)
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (var-set total-warranties-analyzed u0)
        (var-set total-claims-processed u0)
        (ok true)
    )
)

