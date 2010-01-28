#lang scheme/base

(require (planet untyped/unlib:3/bytes)
         "../lib-base.ss"
         "component.ss")

(define-mixin page-mixin (component<%>) (page<%>)
  
  (inherit on-request)
  
  ; Fields -------------------------------------
  
  ; (cell integer)
  (init-cell http-code 200 #:accessor #:mutator)
  
  ; (cell string)
  (init-cell http-status "OK" #:accessor #:mutator)
  
  ; (cell string)
  (init-cell content-type "text/plain" #:accessor #:mutator)
  
  ; Methods ------------------------------------
  
  ; -> integer
  (define/public (get-http-timestamp)
    (current-seconds))
  
  ; -> (listof header)
  (define/public (get-http-headers)
    no-cache-http-headers)
  
  ; Request handling ---------------------------
  
  ; request (-> any) -> any
  ; 
  ; Informs all components that a request has come in before calling thunk.
  (define/public (handle-request request thunk)
    (on-request request)
    (thunk))
  
  ; Response generation ------------------------
  
  ; [#:forward? boolean] -> any
  (define/public (respond)
    (make-plain-response
     #:code      (get-http-code)
     #:status    (get-http-status)
     #:seconds   (get-http-timestamp)
     #:mime-type (ensure-bytes (get-content-type))
     (list "Under construction."))))

; Procedures -------------------------------------

;; any -> boolean
(define (page? item)
  (is-a? item page<%>))

; Provide statements -----------------------------

(provide #;page<%>
         page-mixin
         page?)
