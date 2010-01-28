#lang scheme

(require srfi/19
         web-server/dispatchers/dispatch
         web-server/http
         web-server/http/response
         web-server/private/web-server-structs
         (planet untyped/mirrors:2)
         (planet untyped/unlib:3/log)
         (planet untyped/unlib:3/time)
         "env.ss"
         "web-cell.ss")

; symbol
(define interface-version 'v1)

; (-> response) -> (connection request -> response)
(define (make proc
              #:error-handler   error-handler 
              #:session-expires [expires (void)]
              #:session-domain  [domain #f])
  (lambda (conn req)
    (current-connection-set! conn)
    (current-request-set! req)
    (parameterize ([current-custodian (make-servlet-custodian)])
      (with-handlers ([(lambda (exn) #t) error-handler])
        (output-response/method
         conn
         (proc)
         (request-method req))))))

; Provides ---------------------------------------

(provide/contract
 [interface-version dispatcher-interface-version/c]
 [make              (->* ((-> response/c) #:error-handler (-> exn? response/c))
                         (#:session-expires (or/c time-utc? void? #f) #:session-domain  (or/c string? #f))
                         dispatcher/c)])
