#lang scheme/base

(require (for-syntax scheme/base
                     (planet untyped/unlib:3/syntax))
         (planet untyped/dispatch:2)
         (planet untyped/snooze:3)
         "../../../lib-base.ss"
         "../notification.ss"
         "controller-internal.ss"
         "delete-page.ss"
         "editor-page.ss"
         "report-page.ss"
         "review-page.ss")

(define default-scaffolded-pipeline-maker
  (make-parameter (lambda () null)))

; Syntax -----------------------------------------

(define-syntax (define-report-controller stx)
  (syntax-case stx ()
    [(_ id entity)
     (syntax/loc stx
       (define-report-controller id entity ((default-scaffolded-pipeline-maker))))]
    [(_ id entity pipeline)
     (syntax/loc stx
       (define-report-controller id entity pipeline (scaffold-report-page entity)))]
    [(_ id entity pipeline page)
     (syntax/loc stx
       (begin
         (define the-page page)
         (define-controller id
           pipeline
           (lambda ()
             (send* the-page [respond])))
         (report-controller-set! entity id)))]))

(define-syntax (define-create-controller stx)
  (syntax-case stx ()
    [(_ id entity)
     (syntax/loc stx
       (define-create-controller id entity ((default-scaffolded-pipeline-maker))))]
    [(_ id entity pipeline)
     (syntax/loc stx
       (define-create-controller id entity pipeline (scaffold-create-page entity)))]
    [(_ id entity pipeline page)
     (syntax/loc stx
       (begin
         (define the-page page)
         (define-controller id
           pipeline
           (lambda ()
             (call-review-controller 
              (send* the-page [set-value! (make-snooze-struct/defaults entity)] [respond]))))
         (create-controller-set! entity id)))]))

(define-syntax (define-review-controller stx)
  (syntax-case stx ()
    [(_ id entity)
     (syntax/loc stx
       (define-review-controller id entity ((default-scaffolded-pipeline-maker))))]
    [(_ id entity pipeline)
     (syntax/loc stx
       (define-review-controller id entity pipeline (scaffold-review-page entity)))]
    [(_ id entity pipeline page)
     (syntax/loc stx
       (begin
         (define the-page page)
         (define-controller id
           pipeline
           (lambda (struct)
             (if struct
                 (send* the-page 
                   [set-value! struct] 
                   [respond])
                 (begin (notifications-add! (xml "The " ,(entity-pretty-name entity) " you requested could not be found."))
                        (call-report-controller entity)))))
         (review-controller-set! entity id)))]))

(define-syntax (define-update-controller stx)
  (syntax-case stx ()
    [(_ id entity)
     (syntax/loc stx
       (define-update-controller id entity ((default-scaffolded-pipeline-maker))))]
    [(_ id entity pipeline)
     (syntax/loc stx
       (define-update-controller id entity pipeline (scaffold-update-page entity)))]
    [(_ id entity pipeline page)  
     (syntax/loc stx
       (begin
         (define the-page page)
         (define-controller id
           pipeline
           (lambda (struct)
             (if struct
                 (call-review-controller 
                  (send* the-page 
                    [set-value! struct] 
                    [respond]))
                 (begin (notifications-add! (xml "The " ,(entity-pretty-name entity) " you requested could not be found."))
                        (call-report-controller entity)))))
         (update-controller-set! entity id)))]))

(define-syntax (define-delete-controller stx)
  (syntax-case stx ()
    [(_ id entity)
     (syntax/loc stx
       (define-delete-controller id entity ((default-scaffolded-pipeline-maker))))]
    [(_ id entity pipeline)
     (syntax/loc stx
       (define-delete-controller id entity pipeline (scaffold-delete-page entity)))]
    [(_ id entity pipeline page)  
     (syntax/loc stx
       (begin
         (define the-page page)
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
         (delete-controller-set! entity id)))]))

; Provide statements -----------------------------

(provide (all-from-out "controller-internal.ss")
         define-report-controller
         define-create-controller
         define-review-controller
         define-update-controller
         define-delete-controller)

(provide/contract
 [default-scaffolded-pipeline-maker (parameter/c (-> (listof procedure?)))])