;; Real Estate Investment Analysis Contract
;; Investment evaluation system with market analysis, cash flow projections, risk assessment, and portfolio tracking

;; constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_PROPERTY_NOT_FOUND (err u101))
(define-constant ERR_INVALID_INPUT (err u102))
(define-constant ERR_INSUFFICIENT_FUNDS (err u103))
(define-constant ERR_PORTFOLIO_NOT_FOUND (err u104))

;; data maps and vars
(define-map properties
  { property-id: uint }
  {
    owner: principal,
    purchase-price: uint,
    current-value: uint,
    monthly-rent: uint,
    monthly-expenses: uint,
    property-type: (string-ascii 20),
    location: (string-ascii 50),
    created-at: uint,
    last-updated: uint
  }
)

(define-map market-data
  { location: (string-ascii 50) }
  {
    avg-price-per-sqft: uint,
    rental-yield: uint,
    appreciation-rate: uint,
    market-trend: (string-ascii 10),
    last-updated: uint
  }
)

(define-map portfolios
  { owner: principal }
  {
    total-properties: uint,
    total-value: uint,
    total-monthly-income: uint,
    total-monthly-expenses: uint,
    total-equity: uint,
    roi: uint,
    risk-score: uint
  }
)

(define-map cash-flow-projections
  { property-id: uint, year: uint }
  {
    projected-income: uint,
    projected-expenses: uint,
    net-cash-flow: uint,
    cumulative-return: uint
  }
)

(define-data-var next-property-id uint u1)

;; private functions
(define-private (calculate-roi (income uint) (expenses uint) (investment uint))
  (if (> investment u0)
    (/ (* (- income expenses) u10000) investment)
    u0
  )
)

(define-private (calculate-cap-rate (net-income uint) (property-value uint))
  (if (> property-value u0)
    (/ (* net-income u10000) property-value)
    u0
  )
)

(define-private (calculate-risk-score (property-type (string-ascii 20)) (location (string-ascii 50)) (age uint))
  (let (
    (base-score u50)
    (type-adjustment (if (is-eq property-type "residential") u10
                     (if (is-eq property-type "commercial") u20 u15)))
    (age-adjustment (/ age u5))
  )
    (+ base-score type-adjustment age-adjustment)
  )
)

(define-private (update-portfolio-stats (owner principal))
  (let (
    (current-portfolio (default-to 
      { total-properties: u0, total-value: u0, total-monthly-income: u0, 
        total-monthly-expenses: u0, total-equity: u0, roi: u0, risk-score: u0 }
      (map-get? portfolios { owner: owner })
    ))
  )
    ;; This is a simplified update - in practice would iterate through all properties
    (map-set portfolios { owner: owner } current-portfolio)
  )
)

;; public functions
(define-public (add-property 
  (purchase-price uint)
  (current-value uint) 
  (monthly-rent uint)
  (monthly-expenses uint)
  (property-type (string-ascii 20))
  (location (string-ascii 50))
)
  (let (
    (property-id (var-get next-property-id))
    (current-block stacks-block-height)
  )
    (if (and (> purchase-price u0) (> current-value u0))
      (begin
        (map-set properties
          { property-id: property-id }
          {
            owner: tx-sender,
            purchase-price: purchase-price,
            current-value: current-value,
            monthly-rent: monthly-rent,
            monthly-expenses: monthly-expenses,
            property-type: property-type,
            location: location,
            created-at: current-block,
            last-updated: current-block
          }
        )
        (var-set next-property-id (+ property-id u1))
        (update-portfolio-stats tx-sender)
        (ok property-id)
      )
      ERR_INVALID_INPUT
    )
  )
)

(define-public (update-property-value (property-id uint) (new-value uint))
  (let (
    (property (unwrap! (map-get? properties { property-id: property-id }) ERR_PROPERTY_NOT_FOUND))
  )
    (if (is-eq (get owner property) tx-sender)
      (begin
        (map-set properties
          { property-id: property-id }
          (merge property { current-value: new-value, last-updated: stacks-block-height })
        )
        (update-portfolio-stats tx-sender)
        (ok true)
      )
      ERR_UNAUTHORIZED
    )
  )
)

(define-public (calculate-investment-metrics (property-id uint))
  (let (
    (property (unwrap! (map-get? properties { property-id: property-id }) ERR_PROPERTY_NOT_FOUND))
    (annual-income (* (get monthly-rent property) u12))
    (annual-expenses (* (get monthly-expenses property) u12))
    (net-income (- annual-income annual-expenses))
    (roi (calculate-roi annual-income annual-expenses (get purchase-price property)))
    (cap-rate (calculate-cap-rate net-income (get current-value property)))
    (cash-flow (- (get monthly-rent property) (get monthly-expenses property)))
  )
    (ok {
      roi: roi,
      cap-rate: cap-rate,
      monthly-cash-flow: cash-flow,
      annual-net-income: net-income,
      equity: (- (get current-value property) (get purchase-price property))
    })
  )
)

(define-public (update-market-data
  (location (string-ascii 50))
  (avg-price-per-sqft uint)
  (rental-yield uint)
  (appreciation-rate uint)
  (market-trend (string-ascii 10))
)
  (if (is-eq tx-sender CONTRACT_OWNER)
    (begin
      (map-set market-data
        { location: location }
        {
          avg-price-per-sqft: avg-price-per-sqft,
          rental-yield: rental-yield,
          appreciation-rate: appreciation-rate,
          market-trend: market-trend,
          last-updated: stacks-block-height
        }
      )
      (ok true)
    )
    ERR_UNAUTHORIZED
  )
)

(define-public (create-cash-flow-projection
  (property-id uint)
  (year uint)
  (projected-income uint)
  (projected-expenses uint)
)
  (let (
    (property (unwrap! (map-get? properties { property-id: property-id }) ERR_PROPERTY_NOT_FOUND))
    (net-cash-flow (- projected-income projected-expenses))
  )
    (if (is-eq (get owner property) tx-sender)
      (begin
        (map-set cash-flow-projections
          { property-id: property-id, year: year }
          {
            projected-income: projected-income,
            projected-expenses: projected-expenses,
            net-cash-flow: net-cash-flow,
            cumulative-return: net-cash-flow ;; Simplified - would calculate actual cumulative
          }
        )
        (ok true)
      )
      ERR_UNAUTHORIZED
    )
  )
)

(define-public (get-portfolio-summary (owner principal))
  (let (
    (portfolio (default-to 
      { total-properties: u0, total-value: u0, total-monthly-income: u0, 
        total-monthly-expenses: u0, total-equity: u0, roi: u0, risk-score: u0 }
      (map-get? portfolios { owner: owner })
    ))
  )
    (ok portfolio)
  )
)

;; read-only functions
(define-read-only (get-property (property-id uint))
  (map-get? properties { property-id: property-id })
)

(define-read-only (get-market-data (location (string-ascii 50)))
  (map-get? market-data { location: location })
)

(define-read-only (get-cash-flow-projection (property-id uint) (year uint))
  (map-get? cash-flow-projections { property-id: property-id, year: year })
)

(define-read-only (get-next-property-id)
  (var-get next-property-id)
)
