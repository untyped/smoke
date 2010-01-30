#lang scheme

(require net/url
         srfi/19
         web-server/http
         web-server/http/bindings
         (planet untyped/unlib:3/list)
         (planet untyped/unlib:3/string)
         (planet untyped/unlib:3/time)
         "../base.ss"
         "cookie.ss"
         "env.ss"
         "session-internal.ss")

; Accessing sessions -----------------------------

; request -> (U string #f)
(define (request-session-id request)
  (let ([cookies (ensure-string (assoc-value/default
                                 'cookie
                                 (request-headers request)
                                 #f))])
    (and cookies (get-cookie/single (session-cookie-name) cookies))))

; request -> (U session #f)
(define (request-session request)
  (let/debug ([session-id (request-session-id request)])
    (and session-id
         (let ([session (hash-ref sessions session-id #f)])
           (when session
             (set-session-accessed! session (current-time time-utc)))
           session))))

; Starting, updating and ending sessions ---------

; string string (U time-utc void #f) (U string #f) -> cookie
(define (make-session-cookie name session-id expires domain)
  (let* ([now     (current-time time-utc)]
         [cookie0 (set-cookie name session-id)]
         [cookie1 (cookie:add-path cookie0 "/")]
         [cookie2 (if (time? expires)
                      (cookie:add-expires cookie1 (time-second expires))
                      cookie1)]
         [cookie3 (if domain
                      (cookie:add-domain cookie2 domain)
                      cookie2)])
    cookie3))

; string cookie (U string url) -> response
(define (make-cookie-response message cookie redirect-to)
  (make-redirect-response
   redirect-to
   #:code    302
   #:message message
   #:headers (list* (make-header #"Set-Cookie" (string->bytes/utf-8 (print-cookie cookie)))
                    no-cache-http-headers)))

; Ensures a session is running with the required expiry date.
; If the session cookie is okay, returns #f.
; If the session cookie needs adjusting, returns a 302 response to do the adjustment and do a redirect.
;  [#:expires     (U time-utc #f)]
;  [#:domain      (U string #f)]
;  [#:redirect-to (U url string)]
; ->
;  (U response #f)
(define (start-session
         #:expires     [expires     #f]
         #:domain      [domain      #f]
         #:redirect-to [redirect-to (request-uri (current-request))])
  (cond [(debug* "session" request-session (current-request))
         => (lambda (session)
              (and (not (times-equal? expires (session-expires session)))
                   (make-cookie-response
                    (if (time<? expires (current-time time-utc))
                        "Ending session"
                        "Adjusting session")
                    (make-session-cookie
                     (session-cookie-name)
                     (session-cookie-id session)
                     expires
                     domain)
                    redirect-to)))]
        [else (let ([session (create-session expires)])
                (make-cookie-response
                 "Starting session"
                 (make-session-cookie
                  (session-cookie-name)
                  (session-cookie-id session)
                  expires
                  domain)
                 redirect-to))]))

; -> (U response #f)
(define (end-session)
  (start-session
   #:expires (subtract-duration
              (current-time time-utc)
              one-week)))

; Helpers ----------------------------------------

; time-duration
(define one-week         
  (make-time time-duration 0 (* 60 60 24 7)))

; (U time-utc #f) (U time-utc #f) -> boolean
(define (times-equal? t1 t2)
  (equal? (and t1 (time-second t1))
          (and t2 (time-second t2))))

; Provide statements -----------------------------

(provide (except-out (struct-out session)
                     make-session
                     set-session-expires!)
         session-cookie-name
         session-set?
         session-ref
         session-set!
         session-remove!)

(provide/contract
 [request-session-id (-> request? (or/c string? #f))]
 [request-session    (-> request? (or/c session? #f))]
 [start-session      (->* ()
                                    (#:expires (or/c time-utc? #f)
                                               #:domain (or/c string? #f)
                                               #:redirect-to (or/c string? url?))
                                    (or/c response/c #f))]
 [end-session        (-> (or/c response/c #f))])
