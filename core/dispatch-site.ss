#lang scheme

(require web-server/dispatchers/dispatch
         web-server/http
         (planet untyped/unlib:3/time)
         "env.ss"
         "interfaces.ss"
         (prefix-in proc: "dispatch-proc.ss"))

; symbol
(define interface-version 'v1)

; site -> (connection request -> response)
(define (make site
              #:error-handler   error-handler 
              #:session-expires [expires #f]
              #:session-domain  [domain  #f])
  (proc:make (lambda ()
               (current-site-set! site)
               (send site dispatch/top))
             #:error-handler   error-handler 
             #:session-expires expires
             #:session-domain  domain))

; Provides ---------------------------------------

(provide/contract
 [interface-version dispatcher-interface-version/c]
 [make              (->* ((is-a?/c site<%>) #:error-handler (-> exn? response/c))
                         (#:session-expires (or/c time-utc? #f) #:session-domain  (or/c string? #f))
                         dispatcher/c)])
