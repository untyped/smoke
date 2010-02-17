#lang scheme

(require web-server/dispatchers/dispatch
         web-server/http
         web-server/managers/manager
         (planet untyped/unlib:3/time)
         (prefix-in proc: "dispatch-proc.ss")
         "env.ss"
         "interfaces.ss"
         "lru.ss")

; symbol
(define interface-version 'v1)

; site -> (connection request -> response)
(define (site:make
         site
         #:directory       directory
         #:error-handler   error-handler 
         #:manager         [manager (make-default-smoke-manager)]
         #:session-expires [expires #f]
         #:session-domain  [domain  #f])
  (proc:make (lambda ()
               (current-site-set! site)
               (send site dispatch/top))
             #:directory       directory
             #:error-handler   error-handler
             #:manager         manager
             #:session-expires expires
             #:session-domain  domain))

; Provides ---------------------------------------

(provide/contract
 [interface-version     dispatcher-interface-version/c]
 [rename site:make make (->* ((is-a?/c site<%>)
                              #:directory     path-string?
                              #:error-handler (-> exn? response/c))
                             (#:manager manager? #:session-expires (or/c time-utc? #f) #:session-domain  (or/c string? #f))
                             dispatcher/c)])
