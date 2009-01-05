#lang scheme/base

(require (planet untyped/unlib:3/bytes)
         "../lib-base.ss"
         "component.ss")

(define page<%>
  (interface ()
    get-http-code             ; -> integer
    get-http-headers          ; -> (listof header)
    get-http-status           ; -> string
    get-content-type          ; -> string
    get-http-timestamp        ; -> integer
    handle-request            ; (-> any) -> any
    respond                   ; [#:forward? boolean] -> void
    make-response-generator)) ; web-server-embed-url -> string

(define page-mixin
  (mixin/cells (component<%>) (page<%>)
    
    (inherit on-request)
    
    ; Fields -------------------------------------

    ; (cell integer)
    (init-cell [http-code 200] #:accessor #:mutator)
    
    ; (cell string)
    (init-cell [http-status "OK"] #:accessor #:mutator)
    
    ; (cell string)
    (init-cell [content-type "text/plain"] #:accessor #:mutator)
    
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
    (define/public (respond #:forward? [forward? #f])
      (when forward? (clear-continuation-table!))
      (send/suspend/dispatch (make-response-generator)))
    
    ; -> ((procedure -> string) -> response)
    (define/public (make-response-generator)
      (lambda (embed-url)
        (define seed (make-seed this embed-url))
        (make-response/full
         (get-http-code)
         (get-http-status)
         (get-http-timestamp)
         (ensure-bytes (get-content-type))
         (list "Under construction."))))))

; Procedures -------------------------------------

;; any -> boolean
(define (page? item)
  (is-a? item page<%>))

; Provide statements -----------------------------

(provide page<%>
         page-mixin
         page?)
