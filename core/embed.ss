#lang scheme

(require net/url
         "../base.ss"
         "../class/class.ss"
         (except-in "callback.ss" callback)
         "callback-url.ss")

; Procedures -------------------------------------

; seed (U string callback) -> string
(define (embed/full seed handler)
  (match handler
    [(? string?)   handler]
    [(? callback?) (callback->url seed handler)]))

; seed (U string (-> void) callback) -> string
(define embed embed/full)

; seed (U string (-> void) callback) -> js
(define (embed/ajax seed handler)
  (embed/ajax/delay seed handler))

; seed (U string (-> void) callback) natural -> js
(define (embed/ajax/delay seed handler [delay 100])
  (js (!dot Smoke (doDelayedAjax ,delay ,(embed/full seed handler)))))

; (_ [html-component<%> id] #:arg-name json-serializable ...)
; (_ id #:arg-name json-serializable ...)
(define-syntax callback
  (syntax-rules ()
    [(_ [obj method] arg ...)
     (begin (verify-callback obj 'method)
            (make-callback obj 'method (list arg ...)))]
    [(_ method arg ...)
     (callback [this method] arg ...)]))

; Provide statements -----------------------------

(provide (struct-out seed)
         callback
         callback?)

(provide/contract
 [embed            (-> seed? (or/c string? callback?) (or/c string? url?))]
 [embed/full       (-> seed? (or/c string? callback?) (or/c string? url?))]
 [embed/ajax       (-> seed? (or/c string? callback?) javascript?)]
 [embed/ajax/delay (->* (seed? (or/c string? procedure? callback?))
                        (natural-number/c)
                        javascript?)])
