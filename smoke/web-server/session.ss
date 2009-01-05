#lang scheme/base

(require mzlib/md5
         net/url
         srfi/19
         (planet untyped/mirrors:1/mirrors)
         (planet untyped/unlib:3/cache)
         (planet untyped/unlib:3/list)
         (planet untyped/unlib:3/string)
         "../base.ss"
         "cookie.ss"
         "servlet.ss"
         "session-internal.ss")

; Procedures -------------------------------------

; request -> (U string #f)
(define (request-session-id request)
  (define cookies (ensure-string (assoc-value/default 'cookie (request-headers request) #f)))
  (and cookies (get-cookie/single session-cookie-name cookies)))

; request -> (U session #f)
(define (request-session request)
  (define session-id
    (request-session-id request))
  (and session-id
       (let-debug ([session (hash-ref sessions session-id #f)])
         (when session
           (set-session-accessed! session (current-time time-utc)))
         session)))

; symbol [boolean] [((U session #f) -> any)] -> any
;
; The continuation table is cleared if forward? is #t.
; We normally want this to be the case, but we can't clear the continuation
; table when testing the code with Delirium.
(define (start-session [forward? #t] [continue (lambda (session) session)])
  ; immutable-string
  (define session-id
    (generate-session-id))
  ; time-utc
  (define now
    (current-time time-utc))
  ; session
  (define session 
    (make-session session-id now now (make-hasheq)))
  ; cookie
  (define cookie 
    (cookie:add-path (set-cookie session-cookie-name session-id) "/"))
  ; -> any
  (define (continue-thunk)
    (when forward?
      (clear-continuation-table!))
    (if (equal? session-id (request-session-id (current-request)))
        (hash-set! sessions session-id session)
        (error "session could not be established"))
    (continue session))
  ; any
  (send/suspend/dispatch
   (lambda (embed-url)
     (make-redirect-response
      (embed-url continue-thunk)
      #:code    302
      #:message "Establishing session"
      #:headers (list* (make-header #"Set-Cookie" (string->bytes/utf-8 (print-cookie cookie))) no-cache-http-headers)))))

; (U session string) [boolean] -> void
;
; The continuation table is cleared if forward? is #t.
; We normally want this to be the case, but we can't clear the continuation
; table when testing the code with Delirium.
(define (end-session session+id [forward? #t])
  ; string
  (define session-id
    (if (session? session+id)
        (session-cookie-id session+id)
        session+id))
  ; cookie
  (define cookie 
    (cookie:add-expires 
     (cookie:add-path (set-cookie session-cookie-name session-id) "/")
     (- (current-seconds) (* 7 24 60 60))))
  ; request
  (define request
    (send/suspend/dispatch
     (lambda (embed-url)
       (make-plain-response
        #:code    302
        #:message "Terminating session"
        #:headers (list (make-header #"Set-Cookie" (string->bytes/utf-8 (print-cookie cookie)))
                        (make-header #"Location"   (string->bytes/utf-8 (embed-url (lambda () (current-request))))))
        (list "One moment please: ending your session.")))))
  (when forward?
    (clear-continuation-table!))
  ; session
  (if (request-session-id request)
      (raise-exn exn:fail:smoke:session
        "Session cookie could not be removed.")
      (begin (hash-remove! sessions session-id)
             (void))))

; Helpers ----------------------------------------

; -> string
(define (generate-session-id)
  (string->immutable-string
   (bytes->string/utf-8
    (md5 (string->bytes/utf-8 (number->string (random)))))))

; -> void
(define (default-ensure-session-failure-thunk)
  (error "A session cookie could not be saved: you must have cookies enabled in your browser to view this page."))

; Provide statements -----------------------------

(provide (except-out (struct-out session) make-session)
         session-set?
         session-ref
         session-set!
         session-remove!)

(provide/contract
 [request-session-id (-> request? (or/c string? false/c))]
 [request-session    (-> request? (or/c session? false/c))]
 [start-session      (->* () (boolean? (-> (or/c session? false/c) any)) any)]
 [end-session        (->* ((or/c session? string?)) (boolean?) void?)])

