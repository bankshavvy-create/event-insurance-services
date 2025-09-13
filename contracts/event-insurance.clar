;; Event Insurance Services Smart Contract
;; Manages special event coverage with risk assessment, policy customization, claims processing, and vendor coordination

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-insufficient-payment (err u103))
(define-constant err-policy-expired (err u104))
(define-constant err-claim-already-processed (err u105))
(define-constant err-invalid-status (err u106))
(define-constant err-unauthorized (err u107))
(define-constant err-invalid-risk-level (err u108))

;; Data Variables
(define-data-var next-policy-id uint u1)
(define-data-var next-claim-id uint u1)
(define-data-var next-vendor-id uint u1)

;; Risk levels for assessment
(define-constant risk-low u1)
(define-constant risk-medium u2)
(define-constant risk-high u3)

;; Policy status
(define-constant status-active u1)
(define-constant status-expired u2)
(define-constant status-cancelled u3)

;; Claim status
(define-constant claim-pending u1)
(define-constant claim-approved u2)
(define-constant claim-rejected u3)
(define-constant claim-paid u4)

;; Data Maps

;; Insurance policies
(define-map insurance-policies
  { policy-id: uint }
  {
    policy-holder: principal,
    event-name: (string-ascii 100),
    event-date: uint,
    coverage-amount: uint,
    premium-amount: uint,
    risk-level: uint,
    status: uint,
    created-at: uint,
    expires-at: uint,
    policy-terms: (string-ascii 500)
  }
)

;; Claims
(define-map insurance-claims
  { claim-id: uint }
  {
    policy-id: uint,
    claimant: principal,
    claim-amount: uint,
    claim-description: (string-ascii 500),
    status: uint,
    filed-at: uint,
    processed-at: (optional uint),
    assessor: (optional principal)
  }
)

;; Vendors for event coordination
(define-map event-vendors
  { vendor-id: uint }
  {
    vendor-name: (string-ascii 100),
    vendor-address: principal,
    service-type: (string-ascii 50),
    reputation-score: uint,
    is-active: bool,
    registered-at: uint
  }
)

;; Risk assessment data
(define-map risk-assessments
  { policy-id: uint }
  {
    weather-risk: uint,
    venue-risk: uint,
    attendance-risk: uint,
    equipment-risk: uint,
    overall-score: uint,
    assessed-by: principal,
    assessed-at: uint
  }
)

;; Policy holder -> policy IDs mapping
(define-map user-policies
  { user: principal }
  { policy-ids: (list 50 uint) }
)

;; Public Functions

;; Create new insurance policy
(define-public (create-policy
  (event-name (string-ascii 100))
  (event-date uint)
  (coverage-amount uint)
  (premium-amount uint)
  (policy-terms (string-ascii 500))
)
  (let
    (
      (policy-id (var-get next-policy-id))
      (current-block stacks-block-height)
      (expiry-block (+ current-block u8760)) ;; ~1 year in blocks
    )
    ;; Validate event date is in the future
    (asserts! (> event-date current-block) (err u109))
    
    ;; Create the policy
    (map-set insurance-policies
      { policy-id: policy-id }
      {
        policy-holder: tx-sender,
        event-name: event-name,
        event-date: event-date,
        coverage-amount: coverage-amount,
        premium-amount: premium-amount,
        risk-level: risk-low, ;; Default to low, will be updated by assessment
        status: status-active,
        created-at: current-block,
        expires-at: expiry-block,
        policy-terms: policy-terms
      }
    )
    
    ;; Update user policies list
    (try! (update-user-policies tx-sender policy-id))
    
    ;; Increment policy ID counter
    (var-set next-policy-id (+ policy-id u1))
    
    (ok policy-id)
  )
)

;; Conduct risk assessment for a policy
(define-public (conduct-risk-assessment
  (policy-id uint)
  (weather-risk uint)
  (venue-risk uint)
  (attendance-risk uint)
  (equipment-risk uint)
)
  (let
    (
      (policy-data (unwrap! (map-get? insurance-policies { policy-id: policy-id }) err-not-found))
      (overall-score (+ weather-risk venue-risk attendance-risk equipment-risk))
      (risk-level (if (>= overall-score u12) risk-high
                     (if (>= overall-score u8) risk-medium risk-low)))
    )
    ;; Only policy holder or contract owner can conduct assessment
    (asserts! (or (is-eq tx-sender (get policy-holder policy-data))
                  (is-eq tx-sender contract-owner)) err-unauthorized)
    
    ;; Store risk assessment
    (map-set risk-assessments
      { policy-id: policy-id }
      {
        weather-risk: weather-risk,
        venue-risk: venue-risk,
        attendance-risk: attendance-risk,
        equipment-risk: equipment-risk,
        overall-score: overall-score,
        assessed-by: tx-sender,
        assessed-at: stacks-block-height
      }
    )
    
    ;; Update policy risk level
    (map-set insurance-policies
      { policy-id: policy-id }
      (merge policy-data { risk-level: risk-level })
    )
    
    (ok risk-level)
  )
)

;; File insurance claim
(define-public (file-claim
  (policy-id uint)
  (claim-amount uint)
  (claim-description (string-ascii 500))
)
  (let
    (
      (claim-id (var-get next-claim-id))
      (policy-data (unwrap! (map-get? insurance-policies { policy-id: policy-id }) err-not-found))
    )
    ;; Validate policy exists and is active
    (asserts! (is-eq (get status policy-data) status-active) err-invalid-status)
    
    ;; Validate claimant is policy holder
    (asserts! (is-eq tx-sender (get policy-holder policy-data)) err-unauthorized)
    
    ;; Validate claim amount doesn't exceed coverage
    (asserts! (<= claim-amount (get coverage-amount policy-data)) err-insufficient-payment)
    
    ;; Create the claim
    (map-set insurance-claims
      { claim-id: claim-id }
      {
        policy-id: policy-id,
        claimant: tx-sender,
        claim-amount: claim-amount,
        claim-description: claim-description,
        status: claim-pending,
        filed-at: stacks-block-height,
        processed-at: none,
        assessor: none
      }
    )
    
    ;; Increment claim ID counter
    (var-set next-claim-id (+ claim-id u1))
    
    (ok claim-id)
  )
)

;; Process insurance claim (admin only)
(define-public (process-claim
  (claim-id uint)
  (new-status uint)
)
  (let
    (
      (claim-data (unwrap! (map-get? insurance-claims { claim-id: claim-id }) err-not-found))
    )
    ;; Only contract owner can process claims
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    ;; Validate claim is still pending
    (asserts! (is-eq (get status claim-data) claim-pending) err-claim-already-processed)
    
    ;; Validate new status is valid
    (asserts! (or (is-eq new-status claim-approved)
                  (is-eq new-status claim-rejected)) err-invalid-status)
    
    ;; Update claim
    (map-set insurance-claims
      { claim-id: claim-id }
      (merge claim-data {
        status: new-status,
        processed-at: (some stacks-block-height),
        assessor: (some tx-sender)
      })
    )
    
    (ok true)
  )
)

;; Register event vendor
(define-public (register-vendor
  (vendor-name (string-ascii 100))
  (service-type (string-ascii 50))
)
  (let
    (
      (vendor-id (var-get next-vendor-id))
    )
    ;; Create vendor record
    (map-set event-vendors
      { vendor-id: vendor-id }
      {
        vendor-name: vendor-name,
        vendor-address: tx-sender,
        service-type: service-type,
        reputation-score: u50, ;; Start with neutral score
        is-active: true,
        registered-at: stacks-block-height
      }
    )
    
    ;; Increment vendor ID counter
    (var-set next-vendor-id (+ vendor-id u1))
    
    (ok vendor-id)
  )
)

;; Update vendor reputation (admin only)
(define-public (update-vendor-reputation
  (vendor-id uint)
  (new-score uint)
)
  (let
    (
      (vendor-data (unwrap! (map-get? event-vendors { vendor-id: vendor-id }) err-not-found))
    )
    ;; Only contract owner can update reputation
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    ;; Validate score range (0-100)
    (asserts! (<= new-score u100) (err u110))
    
    ;; Update vendor reputation
    (map-set event-vendors
      { vendor-id: vendor-id }
      (merge vendor-data { reputation-score: new-score })
    )
    
    (ok true)
  )
)

;; Cancel policy (policy holder only)
(define-public (cancel-policy (policy-id uint))
  (let
    (
      (policy-data (unwrap! (map-get? insurance-policies { policy-id: policy-id }) err-not-found))
    )
    ;; Only policy holder can cancel
    (asserts! (is-eq tx-sender (get policy-holder policy-data)) err-unauthorized)
    
    ;; Policy must be active
    (asserts! (is-eq (get status policy-data) status-active) err-invalid-status)
    
    ;; Update policy status
    (map-set insurance-policies
      { policy-id: policy-id }
      (merge policy-data { status: status-cancelled })
    )
    
    (ok true)
  )
)

;; Read-only Functions

;; Get policy details
(define-read-only (get-policy (policy-id uint))
  (map-get? insurance-policies { policy-id: policy-id })
)

;; Get claim details
(define-read-only (get-claim (claim-id uint))
  (map-get? insurance-claims { claim-id: claim-id })
)

;; Get vendor details
(define-read-only (get-vendor (vendor-id uint))
  (map-get? event-vendors { vendor-id: vendor-id })
)

;; Get risk assessment
(define-read-only (get-risk-assessment (policy-id uint))
  (map-get? risk-assessments { policy-id: policy-id })
)

;; Get user policies
(define-read-only (get-user-policies (user principal))
  (default-to { policy-ids: (list) } (map-get? user-policies { user: user }))
)

;; Get contract stats
(define-read-only (get-contract-stats)
  {
    total-policies: (- (var-get next-policy-id) u1),
    total-claims: (- (var-get next-claim-id) u1),
    total-vendors: (- (var-get next-vendor-id) u1)
  }
)

;; Private Functions

;; Helper function to update user policies list
(define-private (update-user-policies (user principal) (policy-id uint))
  (let
    (
      (current-policies (default-to { policy-ids: (list) } (map-get? user-policies { user: user })))
      (updated-list (unwrap! (as-max-len? (append (get policy-ids current-policies) policy-id) u50) (err u111)))
    )
    (map-set user-policies
      { user: user }
      { policy-ids: updated-list }
    )
    (ok true)
  )
)


;; title: event-insurance
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;;

;; data vars
;;

;; data maps
;;

;; public functions
;;

;; read only functions
;;

;; private functions
;;

