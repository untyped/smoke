#lang scheme/base

(require mzlib/md5
         srfi/19
         (planet untyped/unlib:3/list)
         (planet untyped/unlib:3/string)
         (planet untyped/unlib:3/time)
         "../base.ss"
         "cookie.ss"
         "servlet.ss"
         "session-internal.ss"
         "web-cell.ss")

; (web-cell (U string #f))
(define expected-session-id-cell (make-web-cell #f))

; -> (U string #f)
(define (expected-session-id)
  (web-cell-ref expected-session-id-cell))

; (U string #f) -> void
(define (expected-session-id-set! id)
  (web-cell-set! expected-session-id-cell id))

; Procedures -------------------------------------

; request -> (U string #f)
(define (request-session-id request)
  (define cookies (ensure-string (assoc-value/default 'cookie (request-headers request) #f)))
  (and cookies (get-cookie/single (session-cookie-name) cookies)))

; request -> (U session #f)
(define (request-session request)
  (let ([session-id (request-session-id request)])
    (and session-id
         (let ([session (hash-ref sessions session-id #f)])
           (when session
             (set-session-accessed! session (current-time time-utc)))
           session))))

; request -> boolean
(define (request-session-valid? request)
  (let ([session-id  (request-session-id request)]
        [expected-id (expected-session-id)])
    (equal? session-id expected-id)))

; [#:expires (U time-utc #f)] [#:continue (-> any)] -> any
;
; The continuation table is cleared if forward? is #t.
; We normally want this to be the case, but we can't clear the continuation
; table when testing the code with Delirium.
(define (start-session #:expires [expires #f] #:continue [continue void])
  (unless (current-request) 
    (error "no current request"))
  (match (request-session (current-request))
    [#f (let* ([session-id (generate-session-id)]
               [now        (current-time time-utc)]
               [session    (make-session session-id now now expires (make-hasheq))]
               [cookie0    (cookie:add-path (set-cookie (session-cookie-name) session-id) "/")]
               [cookie     (if expires (cookie:add-expires cookie0 (time-second expires)) cookie0)])
          (send/cookie "Establishing session" cookie session-id session (lambda ()
                                                                          (expected-session-id-set! session-id)
                                                                          (continue))))]
    [sess (set-session-expiry expires #:continue (lambda ()
                                                   (unless (expected-session-id)
                                                     (expected-session-id-set! (session-cookie-id sess)))
                                                   (continue)))]))

; (U session symbol) (U time-utc #f) [#:continue (-> any)] -> any
;
; The continuation table is cleared if forward? is #t.
; We normally want this to be the case, but we can't clear the continuation
; table when testing the code with Delirium.
(define (set-session-expiry expires #:continue [continue void])
  (unless (current-request) 
    (error "no current request"))
  (let ([session (request-session (current-request))])
    (if (equal? (and expires (time-second expires)) (and (session-expires session) (time-second (session-expires session))))
        (continue)
        (let* ([session-id (session-cookie-id session)]
               [cookie0    (cookie:add-path (set-cookie (session-cookie-name) session-id) "/")]
               [cookie     (if expires (cookie:add-expires cookie0 (time-second expires)) cookie0)])
          (set-session-expires! session expires)
          (send/cookie "Adjusting session expiry" cookie session-id session continue)))))

; (U session string) [#:continue (-> any)] -> any
;
; The continuation table is cleared if forward? is #t.
; We normally want this to be the case, but we can't clear the continuation
; table when testing the code with Delirium.
(define (end-session #:continue [continue void])
  (unless (current-request) 
    (error "no current request"))
  (let* ([session    (request-session (current-request))]
         [session-id (session-cookie-id session)]
         [cookie     (cookie:add-expires 
                      (cookie:add-path (set-cookie (session-cookie-name) session-id) "/")
                      (- (current-seconds) (* 7 24 60 60)))])
    (send/cookie "Terminating session" cookie session-id #f (lambda ()
                                                              (expected-session-id-set! #f)
                                                              (continue)))))

; Helpers ----------------------------------------

; -> string
(define (generate-session-id)
  (string->immutable-string
   (bytes->string/utf-8
    (md5 (string->bytes/utf-8 (number->string (random)))))))

; The message and the cookie are sent to the client.
; Passing a session struct adds a session to the global hash.
; Passing a session of #f removes the session from the global hash.
; 
; string cookie symbol (U session #f) -> void
(define (send/cookie message cookie session-id session continue)
  (send/suspend/dispatch
   (lambda (embed-url)
     (make-redirect-response
      (embed-url (lambda ()
                   (if (equal? session-id (request-session-id (current-request)))
                       (if session
                           (begin (hash-set! sessions session-id session))
                           (begin (hash-remove! sessions session-id)))
                       (raise-exn exn:fail:smoke:session "Session not established"))
                   (continue)))
      #:code    302
      #:message message
      #:headers (list* (make-header #"Set-Cookie" (string->bytes/utf-8 (print-cookie cookie)))
                       no-cache-http-headers)))))

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
 [request-session-id     (-> request? (or/c string? #f))]
 [request-session        (-> request? (or/c session? #f))]
 [request-session-valid? (-> request? boolean?)]
 [start-session          (->* () (#:continue (-> any) #:expires (or/c time-utc? #f)) any)]
 [set-session-expiry     (->* ((or/c time-utc? #f)) (#:continue (-> any)) any)]
 [end-session            (->* () (#:continue (-> any)) any)])
