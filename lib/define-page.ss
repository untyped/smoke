#lang scheme

(require "../lib-base.ss")

(require (for-syntax syntax/parse
                     (planet untyped/unlib:3/debug)
                     (planet untyped/unlib:3/syntax)
                     "site-info.ss")
         "page.ss"
         "site.ss")

(define-syntax (define-page complete-stx)
  (syntax-parse complete-stx
    [(_ (~describe "page id"
          (~and page-id:id
                (~bind [box-id:id (lookup-page-box #'page-id)])))
        (~describe "expr" expr))
     (syntax/loc complete-stx
       (define page-id
         (let ([ans expr])
           (if (is-a? ans page<%>)
               (box-set! box-id ans)
               (raise-type-error 'define-page "page<%>" ans))
           ans)))]
    [(_ (~describe "id" (~and page-id:id (~bind [class-id:id (make-id #f #'page-id '%)])))
        (~describe "class" superclass)
        (~describe "interfaces" (interface ...))
        clause ...)
     (syntax/loc complete-stx
       (begin
         (define-class class-id superclass (interface ...) clause ...)
         (define-page page-id (new class-id))))]))

; Provides ---------------------------------------

(provide define-page)