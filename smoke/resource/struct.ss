#lang scheme/base

(require net/url
         scheme/contract
         "struct-internal.ss")

; Syntax -----------------------------------------

(define-syntax define-js-resource
  (syntax-rules ()
    [(_ id url (uses ...) kws ...)
     (define id (create-js-resource url (list uses ...) kws ...))]
    [(_ id url kws ...)
     (define id (create-js-resource url null kws ...))]))

(define-syntax define-css-resource
  (syntax-rules ()
    [(_ id url (uses ...) kws ...)
     (define id (create-css-resource url (list uses ...) kws ...))]
    [(_ id url kws ...)
     (define id (create-css-resource url null kws ...))]))

(define-syntax define-compound-resource
  (syntax-rules ()
    [(_ id (uses ...))
     (define id (create-compound-resource (symbol->string (gensym 'id)) (list uses ...)))]))

; Procedures -------------------------------------

; string string [(listof web-resource)] -> js-resource
(define (create-js-resource url [prev null])
  (let ([ans (make-js-resource null null #f (url+string->url url))])
    (for ([prev (in-list (sort-prerequisites prev))])
      (add-web-resource-dependency! ans prev))
    ans))

; string string [(listof web-resource)] -> css-resource
(define (create-css-resource url [prev null] #:media [media null] #:ie-version [ie-version #f])
  (let ([ans (make-css-resource null null #f (url+string->url url) media ie-version)])
    (for ([prev (in-list (sort-prerequisites prev))])
      (add-web-resource-dependency! ans prev))
    ans))

; string (listof web-resource) -> compound-resource
(define (create-compound-resource name prev)
  (let ([ans (make-compound-resource null null #f name)])
    (for ([prev (in-list (sort-prerequisites prev))])
      (add-web-resource-dependency! ans prev))
    ans))

; Helpers ----------------------------------------

; (U url string) -> url
(define (url+string->url url+string)
  (if (url? url+string)
      url+string
      (string->url url+string)))

; (listof web-resource) -> (listof web-resource)
(define (sort-prerequisites prev)
  (sort prev web-resource-dependency?))

; Provide statements -----------------------------

(provide (except-out (all-from-out "struct-internal.ss") 
                     make-web-resource
                     make-js-resource
                     make-css-resource
                     make-compound-resource
                     web-resource-prev
                     web-resource-next
                     web-resource-plan
                     set-web-resource-prev!
                     set-web-resource-next!
                     set-web-resource-plan!
                     web-resource<?
                     web-resource-plan/recreate)
         (rename-out [web-resource-plan/recreate web-resource-plan])
         define-js-resource
         define-css-resource
         define-compound-resource)

(provide/contract
 [rename create-js-resource make-js-resource
         (->* (string?) ((listof web-resource?)) js-resource?)]
 [rename create-css-resource make-css-resource
         (->* (string?)
              ((listof web-resource?)
               #:media      (listof string?)
               #:ie-version (or/c string? false/c))
              css-resource?)]
 [rename create-compound-resource make-compound-resource
         (-> string? (listof web-resource?) compound-resource?)])

