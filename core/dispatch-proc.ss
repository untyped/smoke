#lang scheme

(require web-server/dispatchers/dispatch
         web-server/http
         web-server/managers/manager
         (planet untyped/unlib:3/time)
         "dispatch-proc-servlet.ss"
         "lru.ss"
         "session.ss")

; symbol
(define interface-version 'v1)

; (-> response) -> (connection request -> response)
(define (proc:make
         proc
         #:directory       directory
         #:error-handler   error-handler 
         #:manager         [manager (make-default-smoke-manager)]
         #:session-expires [expires #f]
         #:session-domain  [domain  #f])
  (make/smoke-servlet
   (make-smoke-servlet
    (lambda ()
      (or (start-session #:expires expires #:domain domain)
          (proc)))
    directory
    manager)
   error-handler))

; Provides ---------------------------------------

(provide/contract
 [interface-version     dispatcher-interface-version/c]
 [rename proc:make make (->* ((-> response/c)
                              #:directory     path-string?
                              #:error-handler (-> exn? response/c))
                             (#:manager manager? #:session-expires (or/c time-utc? #f) #:session-domain (or/c string? #f))
                             dispatcher/c)])
