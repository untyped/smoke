#lang scheme

(require web-server/dispatchers/dispatch
         web-server/http
         (planet untyped/unlib:3/time)
         "env.ss"
         "interfaces.ss"
         (prefix-in proc: "dispatch-proc.ss"))

; symbol
(define interface-version 'v1)

; application -> (connection request -> response)
(define (make app
              #:error-handler   error-handler 
              #:session-expires [expires (void)]
              #:session-domain  [domain #f])
  (proc:make (lambda ()
               (current-application-set! app)
               (send app dispatch))
             #:error-handler   error-handler 
             #:session-expires expires
             #:session-domain  domain))

; Provides ---------------------------------------

(provide/contract
 [interface-version dispatcher-interface-version/c]
 [make              (->* ((is-a?/c application<%>) #:error-handler (-> exn? response/c))
                         (#:session-expires (or/c time-utc? void? #f) #:session-domain  (or/c string? #f))
                         dispatcher/c)])
