;; resource-coordination
;; Coordinate volunteers, supplies, and venues for events

;; constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u1100))
(define-constant ERR_RESOURCE_NOT_FOUND (err u1101))
(define-constant ERR_INVALID_DATA (err u1102))
(define-constant ERR_ALREADY_COMMITTED (err u1103))

;; data vars
(define-data-var resource-request-counter uint u0)
(define-data-var volunteer-counter uint u0)

;; data maps
(define-map resource-requests
  uint
  {
    event-id: uint,
    requester: principal,
    resource-type: (string-ascii 30), ;; venue, supplies, equipment, catering
    description: (string-utf8 500),
    quantity-needed: uint,
    deadline: uint,
    budget-allocated: uint,
    status: (string-ascii 20), ;; open, fulfilled, cancelled
    created-at: uint,
    fulfilled-by: (optional principal)
  }
)

(define-map resource-offers
  uint
  {
    resource-request-id: uint,
    provider: principal,
    description: (string-utf8 300),
    quantity-offered: uint,
    cost: uint,
    availability-date: uint,
    contact-info: (string-utf8 200),
    status: (string-ascii 20), ;; pending, accepted, declined
    offered-at: uint
  }
)

(define-map volunteers
  principal
  {
    name: (string-utf8 100),
    skills: (list 10 (string-ascii 30)),
    availability: (list 7 bool),
    contact-info: (string-utf8 200),
    events-volunteered: uint,
    reliability-score: uint,
    is-active: bool
  }
)

(define-map event-volunteer-assignments
  { event-id: uint, volunteer: principal }
  {
    role: (string-ascii 50),
    time-commitment: uint,
    assigned-at: uint,
    confirmed: bool,
    showed-up: (optional bool),
    performance-rating: (optional uint)
  }
)

(define-map venues
  (string-ascii 50)
  {
    name: (string-utf8 100),
    capacity: uint,
    hourly-rate: uint,
    available-amenities: (list 10 (string-ascii 30)),
    contact-person: principal,
    booking-calendar: (list 30 uint), ;; blocked dates
    is-active: bool
  }
)

(define-map equipment-inventory
  (string-ascii 50)
  {
    item-name: (string-utf8 100),
    owner: principal,
    quantity-available: uint,
    rental-rate: uint,
    description: (string-utf8 300),
    maintenance-status: (string-ascii 20),
    is-available: bool
  }
)

;; public functions

;; Register as volunteer
(define-public (register-volunteer
  (name (string-utf8 100))
  (skills (list 10 (string-ascii 30)))
  (availability (list 7 bool))
  (contact-info (string-utf8 200)))
  (begin
    (asserts! (> (len name) u0) ERR_INVALID_DATA)
    (map-set volunteers tx-sender {
      name: name,
      skills: skills,
      availability: availability,
      contact-info: contact-info,
      events-volunteered: u0,
      reliability-score: u100,
      is-active: true
    })
    (var-set volunteer-counter (+ (var-get volunteer-counter) u1))
    (ok true)
  )
)

;; Request resources for an event
(define-public (request-resource
  (event-id uint)
  (resource-type (string-ascii 30))
  (description (string-utf8 500))
  (quantity-needed uint)
  (deadline uint)
  (budget-allocated uint))
  (let (
    (request-id (+ (var-get resource-request-counter) u1))
  )
    (asserts! (> quantity-needed u0) ERR_INVALID_DATA)
    (asserts! (> deadline stacks-block-height) ERR_INVALID_DATA)
    
    (map-set resource-requests request-id {
      event-id: event-id,
      requester: tx-sender,
      resource-type: resource-type,
      description: description,
      quantity-needed: quantity-needed,
      deadline: deadline,
      budget-allocated: budget-allocated,
      status: "open",
      created-at: stacks-block-height,
      fulfilled-by: none
    })
    
    (var-set resource-request-counter request-id)
    (ok request-id)
  )
)

;; Offer resources
(define-public (offer-resource
  (resource-request-id uint)
  (description (string-utf8 300))
  (quantity-offered uint)
  (cost uint)
  (availability-date uint)
  (contact-info (string-utf8 200)))
  (let (
    (request (unwrap! (map-get? resource-requests resource-request-id) ERR_RESOURCE_NOT_FOUND))
  )
    (asserts! (is-eq (get status request) "open") ERR_INVALID_DATA)
    (asserts! (> quantity-offered u0) ERR_INVALID_DATA)
    (asserts! (>= availability-date stacks-block-height) ERR_INVALID_DATA)
    
    (map-set resource-offers resource-request-id {
      resource-request-id: resource-request-id,
      provider: tx-sender,
      description: description,
      quantity-offered: quantity-offered,
      cost: cost,
      availability-date: availability-date,
      contact-info: contact-info,
      status: "pending",
      offered-at: stacks-block-height
    })
    
    (ok true)
  )
)

;; Accept resource offer
(define-public (accept-resource-offer (resource-request-id uint))
  (let (
    (request (unwrap! (map-get? resource-requests resource-request-id) ERR_RESOURCE_NOT_FOUND))
    (offer (unwrap! (map-get? resource-offers resource-request-id) ERR_RESOURCE_NOT_FOUND))
  )
    (asserts! (is-eq tx-sender (get requester request)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status offer) "pending") ERR_INVALID_DATA)
    
    ;; Update offer status
    (map-set resource-offers resource-request-id
      (merge offer { status: "accepted" })
    )
    
    ;; Update request status
    (map-set resource-requests resource-request-id
      (merge request {
        status: "fulfilled",
        fulfilled-by: (some (get provider offer))
      })
    )
    
    (ok true)
  )
)

;; Volunteer for event
(define-public (volunteer-for-event
  (event-id uint)
  (role (string-ascii 50))
  (time-commitment uint))
  (let (
    (volunteer-profile (unwrap! (map-get? volunteers tx-sender) ERR_UNAUTHORIZED))
  )
    (asserts! (get is-active volunteer-profile) ERR_UNAUTHORIZED)
    (asserts! (> time-commitment u0) ERR_INVALID_DATA)
    (asserts! (is-none (map-get? event-volunteer-assignments { event-id: event-id, volunteer: tx-sender })) ERR_ALREADY_COMMITTED)
    
    (map-set event-volunteer-assignments
      { event-id: event-id, volunteer: tx-sender }
      {
        role: role,
        time-commitment: time-commitment,
        assigned-at: stacks-block-height,
        confirmed: false,
        showed-up: none,
        performance-rating: none
      }
    )
    
    (ok true)
  )
)

;; Confirm volunteer assignment
(define-public (confirm-volunteer (event-id uint) (volunteer principal))
  (let (
    (assignment (unwrap! (map-get? event-volunteer-assignments { event-id: event-id, volunteer: volunteer }) ERR_RESOURCE_NOT_FOUND))
  )
    ;; In real implementation, would verify event organizer authority
    (map-set event-volunteer-assignments
      { event-id: event-id, volunteer: volunteer }
      (merge assignment { confirmed: true })
    )
    
    (ok true)
  )
)

;; Register venue
(define-public (register-venue
  (venue-id (string-ascii 50))
  (name (string-utf8 100))
  (capacity uint)
  (hourly-rate uint)
  (available-amenities (list 10 (string-ascii 30))))
  (begin
    (asserts! (> (len name) u0) ERR_INVALID_DATA)
    (asserts! (> capacity u0) ERR_INVALID_DATA)
    
    (map-set venues venue-id {
      name: name,
      capacity: capacity,
      hourly-rate: hourly-rate,
      available-amenities: available-amenities,
      contact-person: tx-sender,
      booking-calendar: (list),
      is-active: true
    })
    
    (ok true)
  )
)

;; Add equipment to inventory
(define-public (add-equipment
  (item-id (string-ascii 50))
  (item-name (string-utf8 100))
  (quantity-available uint)
  (rental-rate uint)
  (description (string-utf8 300)))
  (begin
    (asserts! (> (len item-name) u0) ERR_INVALID_DATA)
    (asserts! (> quantity-available u0) ERR_INVALID_DATA)
    
    (map-set equipment-inventory item-id {
      item-name: item-name,
      owner: tx-sender,
      quantity-available: quantity-available,
      rental-rate: rental-rate,
      description: description,
      maintenance-status: "good",
      is-available: true
    })
    
    (ok true)
  )
)

;; Rate volunteer performance
(define-public (rate-volunteer-performance
  (event-id uint)
  (volunteer principal)
  (showed-up bool)
  (performance-rating uint))
  (let (
    (assignment (unwrap! (map-get? event-volunteer-assignments { event-id: event-id, volunteer: volunteer }) ERR_RESOURCE_NOT_FOUND))
    (volunteer-profile (unwrap! (map-get? volunteers volunteer) ERR_RESOURCE_NOT_FOUND))
  )
    (asserts! (and (>= performance-rating u1) (<= performance-rating u5)) ERR_INVALID_DATA)
    
    ;; Update assignment
    (map-set event-volunteer-assignments
      { event-id: event-id, volunteer: volunteer }
      (merge assignment {
        showed-up: (some showed-up),
        performance-rating: (some performance-rating)
      })
    )
    
    ;; Update volunteer stats
    (let (
      (new-reliability (if showed-up
        (+ (get reliability-score volunteer-profile) u5)
        (let ((score-decrease (- (get reliability-score volunteer-profile) u10)))
          (if (> score-decrease u0) score-decrease u0)
        )
      ))
    )
      (map-set volunteers volunteer
        (merge volunteer-profile {
          events-volunteered: (+ (get events-volunteered volunteer-profile) u1),
          reliability-score: new-reliability
        })
      )
    )
    
    (ok true)
  )
)

;; read only functions

(define-read-only (get-resource-request (request-id uint))
  (map-get? resource-requests request-id)
)

(define-read-only (get-resource-offer (request-id uint))
  (map-get? resource-offers request-id)
)

(define-read-only (get-volunteer (volunteer principal))
  (map-get? volunteers volunteer)
)

(define-read-only (get-volunteer-assignment (event-id uint) (volunteer principal))
  (map-get? event-volunteer-assignments { event-id: event-id, volunteer: volunteer })
)

(define-read-only (get-venue (venue-id (string-ascii 50)))
  (map-get? venues venue-id)
)

(define-read-only (get-equipment (item-id (string-ascii 50)))
  (map-get? equipment-inventory item-id)
)

(define-read-only (get-volunteer-count)
  (var-get volunteer-counter)
)

(define-read-only (get-resource-request-count)
  (var-get resource-request-counter)
)

;; private functions

(define-private (is-valid-resource-type (resource-type (string-ascii 30)))
  (or
    (is-eq resource-type "venue")
    (is-eq resource-type "supplies")
    (is-eq resource-type "equipment")
    (is-eq resource-type "catering")
    (is-eq resource-type "transportation")
    (is-eq resource-type "security")
    (is-eq resource-type "entertainment")
    (is-eq resource-type "decoration")
    (is-eq resource-type "other")
  )
)

