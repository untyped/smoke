#lang scheme/base

(require (for-syntax scheme/base
                     (planet untyped/unlib:3/syntax))
         (planet untyped/dispatch:3)
         (planet untyped/snooze:3)
         "../../../lib-base.ss"
         "../notification.ss"
         "controller-internal.ss"
         "delete-page.ss"
         "editor-page.ss"
         "report-page.ss"
         "review-page.ss")

; Syntax -----------------------------------------

; Parses the (kw ...) part of (define-foo-controller id entity kw ...) returning:
;   - a list of bits of keyword syntax for the controller definition;
;   - a syntax object representing the page.
;
; All keywords are passed through to define-controller except #:page.
;
; The results are returned as a single syntax object of the form ((kw ...) page).
;
; syntax syntax -> syntax
(define-for-syntax (parse-controller-kws complete-stx default-page-stx)
  (define kw-stxs  null) ; accumulated in reverse order
  (define page-stx default-page-stx)
  
  (define (parse-kws stx)
    (syntax-case stx ()
      [() #`(#,(reverse kw-stxs) #,page-stx)]
      
      [(#:page page rest ...)
       (begin (set! page-stx #'page)
              (parse-kws #'(rest ...)))]
      
      [(kw val rest ...)
       (keyword? (syntax->datum #'kw))
       (begin (set! kw-stxs (list* #'val #'kw kw-stxs))
              (parse-kws #'(rest ...)))]))
  
  (parse-kws complete-stx))

(define-syntax (define-report-controller complete-stx)
  (syntax-case complete-stx ()
    [(_ id entity kw ...)
     (with-syntax ([((kw ...) page) (parse-controller-kws #'(kw ...) #'(scaffold-report-page entity))])
       (syntax/loc complete-stx
         (begin
           (define the-page page)
           (define-controller (id)
             kw ...
             (send* the-page [respond]))
           (report-controller-set! entity id))))]))

(define-syntax (define-create-controller complete-stx)
  (syntax-case complete-stx ()
    [(_ id entity kw ...)
     (with-syntax ([((kw ...) page) (parse-controller-kws #'(kw ...) #'(scaffold-create-page entity))])
       (syntax/loc complete-stx
         (begin
           (define the-page page)
           (define-controller (id)
             kw ...
             (call-review-controller (send* the-page [set-value! (make-snooze-struct/defaults entity)] [respond])))
           (create-controller-set! entity id))))]))

(define-syntax (define-review-controller complete-stx)
  (syntax-case complete-stx ()
    [(_ id entity kw ...)
     (with-syntax ([((kw ...) page) (parse-controller-kws #'(kw ...) #'(scaffold-review-page entity))])
       (syntax/loc complete-stx
         (begin
           (define the-page page)
           (define-controller (id struct)
             kw ...
             (if struct
                 (send* the-page 
                   [set-value! struct] 
                   [respond])
                 (begin (notifications-add! (xml "The " ,(entity-pretty-name entity) " you requested could not be found."))
                        (call-report-controller entity))))
           (review-controller-set! entity id))))]))

(define-syntax (define-update-controller complete-stx)
  (syntax-case complete-stx ()
    [(_ id entity kw ...)  
     (with-syntax ([((kw ...) page) (parse-controller-kws #'(kw ...) #'(scaffold-update-page entity))])
       (syntax/loc complete-stx
         (begin
           (define the-page page)
           (define-controller (id struct)
             kw ...
             (if struct
                 (call-review-controller 
                  (send* the-page 
                    [set-value! struct] 
                    [respond]))
                 (begin (notifications-add! (xml "The " ,(entity-pretty-name entity) " you requested could not be found."))
                        (call-report-controller entity))))
           (update-controller-set! entity id))))]))

(define-syntax (define-delete-controller complete-stx)
  (syntax-case complete-stx ()
    [(_ id entity kw ...)  
     (with-syntax ([((kw ...) page) (parse-controller-kws #'(kw ...) #'(scaffold-delete-page entity))])
       (syntax/loc complete-stx
         (begin
           (define the-page page)
           (define-controller (id struct)
             kw ...
             (if struct
                 (begin (send* page 
                          [set-value! struct] 
                          [respond])
                        (call-report-controller entity))
                 (begin (notifications-add! (xml "The " ,(entity-pretty-name entity) " you requested could not be found."))
                        (call-report-controller entity))))
           (delete-controller-set! entity id))))]))

; Provide statements -----------------------------

(provide (all-from-out "controller-internal.ss")
         define-report-controller
         define-create-controller
         define-review-controller
         define-update-controller
         define-delete-controller)
