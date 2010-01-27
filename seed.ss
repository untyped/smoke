#lang web-server

(require (for-syntax scheme/base)
         net/uri-codec
         net/url
         (only-in srfi/13 string-drop string-take)
         (planet untyped/unlib:3/number)
         (planet untyped/unlib:3/string)
         (planet untyped/unlib:3/symbol)
         "base.ss"
         (except-in "seed-internal.ss" callback)
         "json.ss"
         "class/class.ss"
         "web-server/continuation-url.ss")

; Procedures -------------------------------------

; seed (U string (-> void) callback) -> string
(define (embed/full seed handler)
  (cond [(string? handler)    handler]
        [(procedure? handler) ((seed-embed-url seed) 
                               (lambda ()
                                 (send (seed-page seed) handle-request (current-request) handler)
                                 (send (seed-page seed) respond)))]
        [(callback? handler)  (callback-url seed handler)]))

; seed (U string (-> void) callback) -> string
(define embed embed/full)

; seed (U string (-> void) callback) -> js
(define (embed/ajax seed handler)
  (embed/ajax/delay seed handler))

; seed (U string (-> void) callback) natural -> js
(define (embed/ajax/delay seed handler [delay 100])
  (js (!dot Smoke (doDelayedAjax ,delay ,(embed/full seed handler)))))

; seed (-> response) -> string
(define (embed/thunk seed thunk)
  ((seed-embed-url seed) 
   (lambda () 
     (send (seed-page seed) handle-request (current-request) thunk))))

; seed -> (list natural natural natural)
(define (make-callback-codes seed)
  ; string
  (define base-url
    (embed/thunk
     seed
     ; -> any
     (lambda ()
       (let* ([request   (current-request)]
              [page      (seed-page seed)]
              [callback  (request->callback request page)]
              [component (and callback (callback-component callback))])
         (if (and component callback)
             (send (callback-component callback)
                   call-callback
                   (callback-callback-id callback)
                   (callback-args callback))
             (error "no component or callback" (list component callback)))))))
  ; string
  (continuation-url->codes (string->url base-url)))

; (_ [html-component<%> id] #:arg-name json-serializable ...)
; (_ id #:arg-name json-serializable ...)
(define-syntax callback
  (syntax-rules ()
    [(_ [obj method] arg ...) (make-callback obj  (send obj  verify-callback-id 'method) (list arg ...))]
    [(_ method arg ...)       (make-callback this (send this verify-callback-id 'method) (list arg ...))]))

; Provide statements -----------------------------

(provide (struct-out seed)
         callback
         callback?
         current-page)

(provide/contract
 [embed               (-> seed? (or/c string? procedure? callback?) (or/c string? url?))]
 [embed/full          (-> seed? (or/c string? procedure? callback?) (or/c string? url?))]
 [embed/ajax          (-> seed? (or/c string? procedure? callback?) javascript?)]
 [embed/ajax/delay    (->* (seed? (or/c string? procedure? callback?)) (natural-number/c) javascript?)]
 [embed/thunk         (-> seed? procedure? (or/c string? url?))]
 [make-callback-codes (-> seed? (list/c natural? natural? natural?))])
