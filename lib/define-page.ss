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
    [(_ (~describe "id" page-id:id)
        (~describe "superclass-id" superclass-id)
        (~describe "interfaces" (interface ...))
        clause ...)
     (with-syntax ([class-id (make-id #f #'page-id '%)])
       (syntax/loc complete-stx
         (begin
           (define-class class-id superclass-id (interface ...) clause ...)
           (define-page page-id (new class-id)))))]
    [(_ (~describe "page id" page-id:id)
        (~describe "expr" expr))
     (with-syntax ([box-id   (or (lookup-page-box #'page-id)
                               (raise-syntax-error #f "page must also be defined in a define-site expression"
                                                   complete-stx
                                                   #'page-id))]
                   [new-page (syntax->datum #'page-id)])
       (syntax/loc complete-stx
         (define _
           (let ([old-page (unbox box-id)]
                 [new-page expr])
             (if (is-a? new-page page<%>)
                 (set-box! box-id new-page)
                 (raise-type-error 'define-page "page<%>" new-page))
             (send new-page set-site! (send old-page get-site))
             (send old-page set-site! #f)
             new-page))))]))

; Provides ---------------------------------------

(provide define-page)