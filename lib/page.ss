#lang scheme

(require "../lib-base.ss")

(require (planet untyped/unlib:3/bytes)
         "component.ss")

(define-mixin page-mixin (component<%>) (page<%>)
  
  (inherit on-request)
  
  ; Fields -------------------------------------
  
  ; site<%>
  (field site #f #:accessor #:mutator)
  
  ; (cell integer)
  (init-cell http-code 200 #:accessor #:mutator)
  
  ; (cell string)
  (init-cell http-status "OK" #:accessor #:mutator)
  
  ; (cell string)
  (init-cell content-type "text/plain" #:accessor #:mutator)
  
  ; Methods --------------------------------------
  
  ; -> integer
  (define/public (get-http-timestamp)
    (current-seconds))
  
  ; -> (listof header)
  (define/public (get-http-headers)
    no-cache-http-headers)
  
  ; Requests and responses -----------------------
  
  ; any ... -> response
  (define/public (dispatch/top . args)
    (if (access-allowed? . args)
        (begin (on-request (current-request))
               (dispatch . args))
        (begin (access-denied . args))))
  
  ; -> response
  (define/public (dispatch . args)
    (respond))
  
  ; -> response
  (define/public (respond)
    (make-plain-response
     #:code      (get-http-code)
     #:status    (get-http-status)
     #:seconds   (get-http-timestamp)
     #:mime-type (ensure-bytes (get-content-type))
     (list "Under construction.")))
  
    ; Access permissions -------------------------
  
  ; page<%> any ... -> boolean
  (define/public (access-allowed? . args)
    (send/apply site access-allowed? this args))
  
  (define/public (access-denied . args)
    (send/apply site access-denied this args)))

(define-class undefined-page% (page-mixin component%) ()
  
  (inherit-field site)
  
  ; Methods --------------------------------------
  
  ; -> response
  (define/override (dispatch . args)
    (send site page-undefined this . args)))

; Procedures -------------------------------------

; page any ... -> string
(define (controller-url page . args)
  (send (send page get-site) encode-url page args))

; page any ... -> string
(define (controller-access? page . args)
  (send/apply (send page get-site) access-allowed? args))

; Provide statements -----------------------------

(provide page-mixin
         undefined-page%)

(provide/contract
 [controller-url     (->* ((is-a?/c page<%>)) () #:rest any/c string?)]
 [controller-access? (->* ((is-a?/c page<%>)) () #:rest any/c boolean?)])