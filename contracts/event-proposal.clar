;; event-proposal
;; Propose and vote on community events and activities

;; constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u1000))
(define-constant ERR_EVENT_NOT_FOUND (err u1001))
(define-constant ERR_INVALID_DATA (err u1002))
(define-constant ERR_ALREADY_VOTED (err u1003))
(define-constant ERR_VOTING_ENDED (err u1004))

;; data vars
(define-data-var event-counter uint u0)
(define-data-var min-votes-required uint u5)
(define-data-var voting-period uint u1440) ;; ~10 days in blocks

;; data maps
(define-map events
  uint
  {
    proposer: principal,
    title: (string-utf8 200),
    description: (string-utf8 1000),
    event-type: (string-ascii 30),
    proposed-date: uint,
    proposed-duration: uint,
    estimated-attendees: uint,
    venue-requirements: (string-utf8 300),
    budget-estimate: uint,
    status: (string-ascii 20), ;; proposed, approved, rejected, cancelled
    votes-for: uint,
    votes-against: uint,
    voting-deadline: uint,
    created-at: uint,
    approved-at: (optional uint)
  }
)

(define-map event-votes
  { event-id: uint, voter: principal }
  { vote: bool, timestamp: uint }
)

(define-map community-members
  principal
  {
    name: (string-utf8 100),
    reputation: uint,
    events-proposed: uint,
    events-organized: uint,
    votes-cast: uint,
    is-active: bool
  }
)

(define-map event-categories
  (string-ascii 30)
  {
    description: (string-utf8 200),
    default-budget: uint,
    typical-duration: uint,
    requires-permit: bool
  }
)

(define-map event-feedback
  uint
  {
    attendance: uint,
    satisfaction-score: uint,
    feedback-comments: (list 10 (string-utf8 300)),
    success-rating: uint,
    lessons-learned: (string-utf8 500)
  }
)

;; public functions

;; Register as community member
(define-public (register-member (name (string-utf8 100)))
  (begin
    (asserts! (> (len name) u0) ERR_INVALID_DATA)
    (map-set community-members tx-sender {
      name: name,
      reputation: u100,
      events-proposed: u0,
      events-organized: u0,
      votes-cast: u0,
      is-active: true
    })
    (ok true)
  )
)

;; Propose a new event
(define-public (propose-event
  (title (string-utf8 200))
  (description (string-utf8 1000))
  (event-type (string-ascii 30))
  (proposed-date uint)
  (proposed-duration uint)
  (estimated-attendees uint)
  (venue-requirements (string-utf8 300))
  (budget-estimate uint))
  (let (
    (event-id (+ (var-get event-counter) u1))
    (member (unwrap! (map-get? community-members tx-sender) ERR_UNAUTHORIZED))
    (voting-deadline (+ stacks-block-height (var-get voting-period)))
  )
    (asserts! (get is-active member) ERR_UNAUTHORIZED)
    (asserts! (> (len title) u0) ERR_INVALID_DATA)
    (asserts! (> proposed-date stacks-block-height) ERR_INVALID_DATA)
    (asserts! (> estimated-attendees u0) ERR_INVALID_DATA)
    
    (map-set events event-id {
      proposer: tx-sender,
      title: title,
      description: description,
      event-type: event-type,
      proposed-date: proposed-date,
      proposed-duration: proposed-duration,
      estimated-attendees: estimated-attendees,
      venue-requirements: venue-requirements,
      budget-estimate: budget-estimate,
      status: "proposed",
      votes-for: u0,
      votes-against: u0,
      voting-deadline: voting-deadline,
      created-at: stacks-block-height,
      approved-at: none
    })
    
    ;; Update proposer statistics
    (map-set community-members tx-sender
      (merge member { events-proposed: (+ (get events-proposed member) u1) })
    )
    
    (var-set event-counter event-id)
    (ok event-id)
  )
)

;; Vote on an event proposal
(define-public (vote-on-event (event-id uint) (vote bool))
  (let (
    (event (unwrap! (map-get? events event-id) ERR_EVENT_NOT_FOUND))
    (member (unwrap! (map-get? community-members tx-sender) ERR_UNAUTHORIZED))
  )
    (asserts! (get is-active member) ERR_UNAUTHORIZED)
    (asserts! (< stacks-block-height (get voting-deadline event)) ERR_VOTING_ENDED)
    (asserts! (is-eq (get status event) "proposed") ERR_VOTING_ENDED)
    (asserts! (is-none (map-get? event-votes { event-id: event-id, voter: tx-sender })) ERR_ALREADY_VOTED)
    
    ;; Record the vote
    (map-set event-votes
      { event-id: event-id, voter: tx-sender }
      { vote: vote, timestamp: stacks-block-height }
    )
    
    ;; Update vote counts
    (let (
      (updated-event 
        (if vote
          (merge event { votes-for: (+ (get votes-for event) u1) })
          (merge event { votes-against: (+ (get votes-against event) u1) })
        )
      )
      (total-votes (+ (get votes-for updated-event) (get votes-against updated-event)))
    )
      (map-set events event-id updated-event)
      
      ;; Update member vote count
      (map-set community-members tx-sender
        (merge member { votes-cast: (+ (get votes-cast member) u1) })
      )
      
      ;; Auto-approve if minimum votes reached and majority approves
      (if (>= total-votes (var-get min-votes-required))
        (if (> (get votes-for updated-event) (get votes-against updated-event))
          (begin
            (map-set events event-id
              (merge updated-event {
                status: "approved",
                approved-at: (some stacks-block-height)
              })
            )
            (ok "approved")
          )
          (begin
            (map-set events event-id
              (merge updated-event { status: "rejected" })
            )
            (ok "rejected")
          )
        )
        (ok "vote-recorded")
      )
    )
  )
)

;; Add event category
(define-public (add-event-category
  (category-name (string-ascii 30))
  (description (string-utf8 200))
  (default-budget uint)
  (typical-duration uint)
  (requires-permit bool))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set event-categories category-name {
      description: description,
      default-budget: default-budget,
      typical-duration: typical-duration,
      requires-permit: requires-permit
    })
    (ok true)
  )
)

;; Submit event feedback
(define-public (submit-feedback
  (event-id uint)
  (attendance uint)
  (satisfaction-score uint)
  (success-rating uint)
  (lessons-learned (string-utf8 500)))
  (let (
    (event (unwrap! (map-get? events event-id) ERR_EVENT_NOT_FOUND))
  )
    (asserts! (is-eq tx-sender (get proposer event)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status event) "approved") ERR_INVALID_DATA)
    (asserts! (and (>= satisfaction-score u1) (<= satisfaction-score u5)) ERR_INVALID_DATA)
    (asserts! (and (>= success-rating u1) (<= success-rating u5)) ERR_INVALID_DATA)
    
    (map-set event-feedback event-id {
      attendance: attendance,
      satisfaction-score: satisfaction-score,
      feedback-comments: (list),
      success-rating: success-rating,
      lessons-learned: lessons-learned
    })
    
    ;; Update organizer reputation based on success
    (let (
      (member (unwrap-panic (map-get? community-members tx-sender)))
      (reputation-change (if (>= success-rating u4) u10 u0))
    )
      (map-set community-members tx-sender
        (merge member {
          events-organized: (+ (get events-organized member) u1),
          reputation: (+ (get reputation member) reputation-change)
        })
      )
    )
    
    (ok true)
  )
)

;; Cancel event (by proposer or admin)
(define-public (cancel-event (event-id uint) (reason (string-utf8 300)))
  (let (
    (event (unwrap! (map-get? events event-id) ERR_EVENT_NOT_FOUND))
  )
    (asserts!
      (or
        (is-eq tx-sender (get proposer event))
        (is-eq tx-sender CONTRACT_OWNER)
      )
      ERR_UNAUTHORIZED
    )
    (asserts!
      (or
        (is-eq (get status event) "proposed")
        (is-eq (get status event) "approved")
      )
      ERR_INVALID_DATA
    )
    
    (map-set events event-id
      (merge event { status: "cancelled" })
    )
    
    (ok true)
  )
)

;; read only functions

(define-read-only (get-event (event-id uint))
  (map-get? events event-id)
)

(define-read-only (get-member (member principal))
  (map-get? community-members member)
)

(define-read-only (get-event-vote (event-id uint) (voter principal))
  (map-get? event-votes { event-id: event-id, voter: voter })
)

(define-read-only (get-event-category (category-name (string-ascii 30)))
  (map-get? event-categories category-name)
)

(define-read-only (get-event-feedback (event-id uint))
  (map-get? event-feedback event-id)
)

(define-read-only (get-event-count)
  (var-get event-counter)
)

(define-read-only (is-voting-active (event-id uint))
  (match (map-get? events event-id)
    event
      (and
        (is-eq (get status event) "proposed")
        (< stacks-block-height (get voting-deadline event))
      )
    false
  )
)

;; private functions

(define-private (is-valid-event-type (event-type (string-ascii 30)))
  (or
    (is-eq event-type "festival")
    (is-eq event-type "fundraiser")
    (is-eq event-type "workshop")
    (is-eq event-type "meeting")
    (is-eq event-type "cleanup")
    (is-eq event-type "celebration")
    (is-eq event-type "sports")
    (is-eq event-type "educational")
    (is-eq event-type "cultural")
    (is-eq event-type "other")
  )
)

