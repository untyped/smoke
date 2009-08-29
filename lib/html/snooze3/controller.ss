#lang scheme/base

(require (for-syntax scheme/base
                     scheme/match
                     syntax/parse
                     (planet cce/scheme:4/syntax)
                     (only-in (planet untyped/snooze:3) entity-info-ref)
                     (planet untyped/unlib:3/debug)
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

; Helpers ----------------------------------------

;  syntax             ; #'(define-report-controller etc ...)
;  syntax             ; #'scaffold-report-page
;  syntax             ; #'report-controller-set!
;  (syntax -> syntax) ; (#'page-id -> #'(send* page-id ...))
(define-for-syntax (make-controller-definition
                    complete-stx
                    scaffold-page-stx
                    register-stx
                    page->body)
  (with-syntax ([scaffold-page   scaffold-page-stx]
                [controller-set! register-stx])
    (syntax-parse complete-stx
      
      ; (_ (id arg ...)
      ;   #:entity entity
      ;   [#:page page]
      ;   [#:other-kw other-val] ...
      ;   [expr ...])
      [(_ (~describe "function name and arguments" (id:id arg ...))
          (~or (~once (~seq #:entity entity) #:name "#:entity argument" #:too-few "missing #:entity argument" #:too-many "multiple #:entity arguments")
               (~optional (~seq #:page page) #:name "#:page argument" #:too-many "multiple #:page arguments")
               (~seq kw:keyword val:expr)) ...
                                           body-expr:expr ...)
       
       (with-syntax* ([page-id                     (make-id #f #'id '-page)]
                      [page                        (or (attribute page) #'(scaffold-page entity))]
                      [(kw+val ...)                (apply append (map list
                                                                      (syntax->list #'(kw ...))
                                                                      (syntax->list #'(val ...))))]
                      [define-page                 (if (pair? (attribute body-expr))
                                                       #'(begin)
                                                       #'(define page-id page))]
                      [body                        (if (pair? (attribute body-expr))
                                                       #'(begin body-expr ...)
                                                       (page->body #'entity #'page-id))])
         
         (syntax/loc complete-stx
           (begin define-page
                   (define-controller (id arg ...) kw+val ... body)
                   (controller-set! entity id))))]
      
      ; (_ id entity
      ;   [#:page page]
      ;   [#:other-kw other-val] ...)
      [(_ id:id 
          (~and entity:id (~fail #:unless (entity-info-ref #'entity) "missing entity"))
          (~or (~optional (~seq #:page page) #:name "#:page argument" #:too-many "multiple #:page arguments")
               (~seq kw:keyword val:expr)) ...)
       
       (with-syntax* ([page-id                          (make-id #f #'id '-page)]
                      [page                             (or (attribute page) #'(scaffold-page entity))]
                      [(kw+val ...)                     (apply append (map list
                                                                           (syntax->list #'(kw ...))
                                                                           (syntax->list #'(val ...))))]
                      [define-page                      #'(define page-id page)]
                      [(lambda (arg ...) body-expr ...) (page->body #'entity #'page-id)])
         
         (syntax/loc complete-stx
           (begin define-page
                   (define-controller (id arg ...) kw+val ... body-expr ...)
                   (controller-set! entity id))))])))

; Syntax -----------------------------------------

(define-syntax (define-report-controller complete-stx)
  (make-controller-definition
   complete-stx
   #'scaffold-report-page
   #'report-controller-set!
   (lambda (entity-stx page-stx)
     (with-syntax ([entity entity-stx] [page page-stx])
       #'(lambda ()
           (send* page [respond]))))))

(define-syntax (define-create-controller complete-stx)
  (make-controller-definition
   complete-stx
   #'scaffold-create-page
   #'create-controller-set!
   (lambda (entity-stx page-stx)
     (with-syntax ([entity entity-stx] [page page-stx])
       #'(lambda ()
           (call-review-controller
            (send* page
              [set-value! (make-snooze-struct/defaults entity)]
              [respond])))))))

(define-syntax (define-review-controller complete-stx)
  (make-controller-definition
   complete-stx
   #'scaffold-review-page
   #'review-controller-set!
   (lambda (entity-stx page-stx)
     (with-syntax ([entity entity-stx] [page page-stx])
       #'(lambda (struct)
           (if struct
               (begin (send* page
                        [set-value! struct] 
                        [respond]))
               (begin (notifications-add! (xml "The " ,(entity-pretty-name entity) " you requested could not be found."))
                      (call-report-controller entity))))))))

(define-syntax (define-update-controller complete-stx)
  (make-controller-definition
   complete-stx
   #'scaffold-update-page
   #'update-controller-set!
   (lambda (entity-stx page-stx)
     (with-syntax ([entity entity-stx] [page page-stx])
       #'(lambda (struct)
           (if struct
               (begin (call-review-controller 
                       (send* page 
                         [set-value! struct] 
                         [respond])))
               (begin (notifications-add! (xml "The " ,(entity-pretty-name entity) " you requested could not be found."))
                      (call-report-controller entity))))))))

(define-syntax (define-delete-controller complete-stx)
  (make-controller-definition
   complete-stx
   #'scaffold-delete-page
   #'delete-controller-set!
   (lambda (entity-stx page-stx)
     (with-syntax ([entity entity-stx] [page page-stx])
       #'(lambda (struct)
           (if struct
               (begin (send* page 
                        [set-value! struct] 
                        [respond])
                      (call-report-controller entity))
               (begin (notifications-add! (xml "The " ,(entity-pretty-name entity) " you requested could not be found."))
                      (call-report-controller entity))))))))

; Provide statements -----------------------------

(provide (all-from-out "controller-internal.ss")
         define-report-controller
         define-create-controller
         define-review-controller
         define-update-controller
         define-delete-controller)
