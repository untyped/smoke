#lang scheme

(require "../lib-base.ss")

(require syntax/boundmap)

; Info types -------------------------------------

(define-struct site-info
  (id private-id page-ids)
  #:transparent
  #:property
  prop:procedure
  (lambda (info stx)
    (syntax-case stx ()
      [id (identifier? #'id)
          (site-info-private-id info)])))

(define-struct page-info
  (id box-id)
  #:transparent
  #:property
  prop:procedure
  (lambda (info stx)
    (syntax-case stx ()
      [id (identifier? #'id)
          #'(unbox (page-info-box-id page))])))

; Caches -----------------------------------------

(define site-info-cache (make-module-identifier-mapping))
(define page-info-cache (make-module-identifier-mapping))

; Procedures -------------------------------------

; site-info -> site-info
(define (site-info-add! info)
  (module-identifier-mapping-put! site-info-cache (site-info-id info) info)
  info)

; identifier -> boolean
(define (site-info-set? id)
  (with-handlers ([exn? (lambda _ #f)])
    (module-identifier-mapping-get site-info-cache id) 
    #t))

; identifier -> site-info
(define (site-info-ref id)
  (module-identifier-mapping-get site-info-cache id))

; page-info -> page-info
(define (page-info-add! info)
  (module-identifier-mapping-put! page-info-cache (page-info-id info) info)
  info)

; identifier -> boolean
(define (page-info-set? id)
  (with-handlers ([exn? (lambda _ #f)])
    (module-identifier-mapping-get page-info-cache id) 
    #t))

; identifier -> page-info
(define (page-info-ref id)
  (module-identifier-mapping-get page-info-cache id))

; identifier -> (U identifier #f)
(define (lookup-page-box id-stx)
  (with-handlers ([exn? (lambda _ #f)])
    (page-info-box-id (page-info-ref id-stx))))

; Provide statements -----------------------------

(provide (struct-out site-info)
         (struct-out page-info))

(provide/contract
 [site-info-add!  (-> site-info? site-info?)]
 [site-info-set?  (-> identifier? boolean?)]
 [site-info-ref   (-> identifier? (or/c site-info? #f))]
 [page-info-add!  (-> page-info? page-info?)]
 [page-info-set?  (-> identifier? boolean?)]
 [page-info-ref   (-> identifier? (or/c page-info? #f))]
 [lookup-page-box (-> identifier? (or/c identifier? #f))])
