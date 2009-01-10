#lang scheme/base

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
  (cond [(string? handler)    (js ((!dot Smoke doAjax) ,(embed/full seed handler)))]
        [(procedure? handler) (js ((!dot Smoke doAjax) ,(embed/full seed handler)))]
        [(callback? handler)  (js ((!dot Smoke doAjax) ,(callback-url seed handler)))]))

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
       ; request
       (define request (current-request))
       ; html-page<%>
       (define page (seed-page seed))
       ; callback
       (define callback (request->callback request page))
       (send (callback-component callback)
             call-callback
             (callback-callback-id callback)
             (callback-args callback)))))
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
 [embed/thunk         (-> seed? procedure? (or/c string? url?))]
 [make-callback-codes (-> seed? (list/c natural? natural? natural?))])
