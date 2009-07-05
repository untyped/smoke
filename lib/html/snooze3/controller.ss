#lang scheme/base

(require (for-syntax scheme/base
                     (planet untyped/unlib:3/syntax))
         (planet untyped/dispatch:2)
         (planet untyped/snooze:3)
         "../../../lib-base.ss"
         "../notification.ss")

; Caches -----------------------------------------

; (hashof entity controller)
(define report-controllers (make-hasheq))
(define create-controllers (make-hasheq))
(define review-controllers (make-hasheq))
(define update-controllers (make-hasheq))
(define delete-controllers (make-hasheq))

; Procedures -------------------------------------

; (U snooze-struct entity) -> controller
(define (report-controller-ref struct+entity) (hash-ref report-controllers (struct+entity->entity struct+entity)))
(define (create-controller-ref struct+entity) (hash-ref create-controllers (struct+entity->entity struct+entity)))
(define (review-controller-ref struct+entity) (hash-ref review-controllers (struct+entity->entity struct+entity)))
(define (update-controller-ref struct+entity) (hash-ref update-controllers (struct+entity->entity struct+entity)))
(define (delete-controller-ref struct+entity) (hash-ref delete-controllers (struct+entity->entity struct+entity)))

; (U snooze-struct entity) -> boolean
(define (report-controller-set? struct+entity) (and (hash-ref report-controllers (struct+entity->entity struct+entity) #f) #t))
(define (create-controller-set? struct+entity) (and (hash-ref create-controllers (struct+entity->entity struct+entity) #f) #t))
(define (review-controller-set? struct+entity) (and (hash-ref review-controllers (struct+entity->entity struct+entity) #f) #t))
(define (update-controller-set? struct+entity) (and (hash-ref update-controllers (struct+entity->entity struct+entity) #f) #t))
(define (delete-controller-set? struct+entity) (and (hash-ref delete-controllers (struct+entity->entity struct+entity) #f) #t))

; entity controller -> void
(define (report-controller-set! entity controller) (hash-set! report-controllers entity controller))
(define (create-controller-set! entity controller) (hash-set! create-controllers entity controller))
(define (review-controller-set! entity controller) (hash-set! review-controllers entity controller))
(define (update-controller-set! entity controller) (hash-set! update-controllers entity controller))
(define (delete-controller-set! entity controller) (hash-set! delete-controllers entity controller))

; entity -> string
(define (report-controller-url entity) (controller-url (report-controller-ref entity)))
(define (create-controller-url entity) (controller-url (create-controller-ref entity)))

; snooze-struct -> string
(define (review-controller-url struct) (controller-url (review-controller-ref (snooze-struct-entity struct)) struct))
(define (update-controller-url struct) (controller-url (update-controller-ref (snooze-struct-entity struct)) struct))
(define (delete-controller-url struct) (controller-url (delete-controller-ref (snooze-struct-entity struct)) struct))

; entity -> any
(define (call-report-controller entity) ((report-controller-ref entity)))
(define (call-create-controller entity) ((create-controller-ref entity)))

; snooze-struct -> any
(define (call-review-controller struct) ((review-controller-ref (snooze-struct-entity struct)) struct))
(define (call-update-controller struct) ((update-controller-ref (snooze-struct-entity struct)) struct))
(define (call-delete-controller struct) ((delete-controller-ref (snooze-struct-entity struct)) struct))

; Helpers ----------------------------------------

; (U snooze-struct entity) -> entity
(define (struct+entity->entity struct+entity)
  (if (entity? struct+entity)
      struct+entity
      (snooze-struct-entity struct+entity)))

; Syntax -----------------------------------------

(define-syntax-rule (define-report-controller id entity pipeline page)
  (begin
    (define-controller id
      pipeline
      (lambda ()
        (send* page [respond])))
    (report-controller-set! entity id)))

(define-syntax-rule (define-create-controller id entity pipeline page)
  (begin
    (define-controller id
      pipeline
      (lambda ()
        (call-review-controller 
         (send* page 
           [set-value! (make-snooze-struct/defaults entity)] 
           [respond]))))
    (create-controller-set! entity id)))

(define-syntax-rule (define-review-controller id entity pipeline page)
  (begin
    (define-controller id
      pipeline
      (lambda (struct)
        (if struct
            (send* page 
              [set-value! struct] 
              [respond])
            (begin (notifications-add! (xml "The " ,(entity-pretty-name entity) " you requested could not be found."))
                   (call-report-controller entity)))))
    (review-controller-set! entity id)))

(define-syntax-rule (define-update-controller id entity pipeline page)
  (begin
    (define-controller id
      pipeline
      (lambda (struct)
        (if struct
            (call-review-controller 
             (send* page 
               [set-value! struct] 
               [respond]))
            (begin (notifications-add! (xml "The " ,(entity-pretty-name entity) " you requested could not be found."))
                   (call-report-controller entity)))))
    (update-controller-set! entity id)))

(define-syntax-rule (define-delete-controller id entity pipeline page)
  (begin
    (define-controller id
      pipeline
      (lambda (struct)
        (if struct
            (begin (send* page 
                     [set-value! struct] 
                     [respond])
                   (call-report-controller entity))
            (begin (notifications-add! (xml "The " ,(entity-pretty-name entity) " you requested could not be found."))
                   (call-report-controller entity)))))
    (delete-controller-set! entity id)))

(define-syntax-rule (define-all-controllers (report-id create-id review-id update-id delete-id)
                      entity
                      pipeline
                      report-page
                      create-page
                      review-page
                      update-page
                      delete-page)
  (begin (define-report-controller report-id entity pipeline report-page)
         (define-create-controller create-id entity pipeline create-page)
         (define-review-controller review-id entity pipeline review-page)
         (define-update-controller update-id entity pipeline update-page)
         (define-delete-controller delete-id entity pipeline delete-page)))

; Provide statements -----------------------------

(provide define-report-controller
         define-create-controller
         define-review-controller
         define-update-controller
         define-delete-controller
         define-all-controllers)

(provide/contract
 [report-controller-ref  (-> (or/c snooze-struct? entity?) (or/c procedure? #f))]
 [create-controller-ref  (-> (or/c snooze-struct? entity?) (or/c procedure? #f))]
 [review-controller-ref  (-> (or/c snooze-struct? entity?) (or/c procedure? #f))]
 [update-controller-ref  (-> (or/c snooze-struct? entity?) (or/c procedure? #f))]
 [delete-controller-ref  (-> (or/c snooze-struct? entity?) (or/c procedure? #f))]
 [report-controller-set? (-> (or/c snooze-struct? entity?) boolean?)]
 [create-controller-set? (-> (or/c snooze-struct? entity?) boolean?)]
 [review-controller-set? (-> (or/c snooze-struct? entity?) boolean?)]
 [update-controller-set? (-> (or/c snooze-struct? entity?) boolean?)]
 [delete-controller-set? (-> (or/c snooze-struct? entity?) boolean?)]
 [report-controller-set! (-> entity? procedure? void?)]
 [create-controller-set! (-> entity? procedure? void?)]
 [review-controller-set! (-> entity? procedure? void?)]
 [update-controller-set! (-> entity? procedure? void?)]
 [delete-controller-set! (-> entity? procedure? void?)]
 [report-controller-url  (-> entity? any)]
 [create-controller-url  (-> entity? any)]
 [review-controller-url  (-> snooze-struct? any)]
 [update-controller-url  (-> snooze-struct? any)]
 [delete-controller-url  (-> snooze-struct? any)]
 [call-report-controller (-> entity? any)]
 [call-create-controller (-> entity? any)]
 [call-review-controller (-> snooze-struct? any)]
 [call-update-controller (-> snooze-struct? any)]
 [call-delete-controller (-> snooze-struct? any)])
