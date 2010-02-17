#lang scheme

(require net/url
         "../base.ss"
         "../class/class.ss"
         (except-in "callback.ss" callback)
         "env.ss"
         "send.ss")

; Structures -------------------------------------

; (struct)
(define-struct seed () #:transparent)

; (struct ((-> response) -> url-string))
(define-struct (k-seed seed) (embed/url) #:transparent)

; Procedures -------------------------------------

; seed (U string callback) -> string
(define (embed/full seed handler)
  (match handler
    [(? string?) handler]
    [(? callback?)
     (let ([base-url (callback->url handler)])
       (if (k-seed? seed)
           ((k-seed-embed/url seed)
            (lambda ()
              (send (current-site) dispatch-callback handler))
            base-url)
           base-url))]
    [(? procedure?)
     (if (k-seed? seed)
         ((k-seed-embed/url seed) handler)
         (raise-exn exn:fail:smoke:callback
           "cannot embed continuations: not in send/suspend/dispatch"))]))

; seed (U string (-> void) callback) -> string
(define embed embed/full)

; seed (U string (-> void) callback) natural -> js
(define (embed/ajax/delay seed handler [delay 100])
  (js (!dot Smoke (doDelayedAjax ,delay ,(embed/full seed handler)))))

; seed (U string (-> void) callback) -> js
(define embed/ajax embed/ajax/delay)

; (_ [html-component<%> id] #:arg-name json-serializable ...)
; (_ id #:arg-name json-serializable ...)
(define-syntax callback
  (syntax-rules ()
    [(_ [obj method] arg ...)
     (begin (send obj verify-callback 'method)
            (make-callback obj 'method (list arg ...)))]
    [(_ method arg ...)
     (callback [this method] arg ...)]))

; Provide statements -----------------------------

(provide (all-from-out "callback.ss")
         callback)

(provide/contract
 [struct seed          ()]
 [struct (k-seed seed) ([embed/url smoke-embed/url/c])]
 [embed                (-> seed? (or/c string? callback?) (or/c string? url?))]
 [embed/full           (-> seed? (or/c string? callback?) (or/c string? url?))]
 [embed/ajax           (-> seed? (or/c string? callback?) javascript?)]
 [embed/ajax/delay     (->* (seed? (or/c string? procedure? callback?))
                            (natural-number/c)
                            javascript?)])
