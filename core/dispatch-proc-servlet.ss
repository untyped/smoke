#lang scheme

(require web-server/dispatchers/dispatch
         web-server/http
         web-server/http/response
         web-server/managers/manager
         web-server/private/connection-manager
         web-server/private/servlet
         web-server/private/web-server-structs
         web-server/servlet/web
         "env.ss"
         "lru.ss"
         "web-cell.ss")

; Dispatcher -------------------------------------

; servlet (exn -> response) -> (connection request -> void)
(define (make/smoke-servlet servlet error-handler)
  (lambda (conn req)
    (current-connection-set! conn)
    (current-request-set! req)
    (let ([instance-custodian (make-servlet-custodian)])
      (parameterize ([current-custodian         instance-custodian]
                     [current-execution-context (make-execution-context req)]
                     [exit-handler              (lambda _
                                                  (kill-connection! conn)
                                                  (custodian-shutdown-all instance-custodian))])
        (let ([res (parameterize ([current-servlet   servlet]
                                  [current-custodian (servlet-custodian servlet)]
                                  [current-directory (servlet-directory servlet)]
                                  [current-namespace (servlet-namespace servlet)])
                     (with-new-web-frame
                      (with-handlers ([exn:dispatcher? raise]
                                      [(lambda (x) #t) error-handler])
                        (call-with-continuation-barrier 
                         (lambda ()
                           (call-with-continuation-prompt
                            (lambda ()
                              ((servlet-handler servlet) req))
                            servlet-prompt))))))])
          (output-response conn res))))))

; Servlet ----------------------------------------

;  (-> response)
;  path-string
;  [manager]
;  [custodian]
;  [namespace]
; ->
;  servlet
(define (make-smoke-servlet
         proc
         directory
         [manager   (make-default-smoke-manager)]
         [custodian (current-custodian)]
         [namespace (current-namespace)])
  (make-servlet 
   custodian
   namespace
   manager
   directory
   ; request -> response
   (let ([start (lambda (request) (proc))])
     (lambda (req)
       (with-handlers ([exn:fail:servlet-manager:no-instance?
                        (lambda (exn) ((exn:fail:servlet-manager:no-instance-expiration-handler exn) req))]
                       [exn:fail:servlet-manager:no-continuation?
                        (lambda (exn) ((exn:fail:servlet-manager:no-continuation-expiration-handler exn) req))])
         (define uri (request-uri req))
         (define-values (instance-id handler)
           (cond [(continuation-url? uri)
                  => (match-lambda
                       [(list instance-id k-id salt)
                        (values instance-id
                                (custodian-box-value
                                 ((manager-continuation-lookup manager) instance-id k-id salt)))])]
                 [else (values ((manager-create-instance manager) (exit-handler))
                               (lambda (request)
                                 (proc)))]))
         (parameterize ([current-servlet-instance-id instance-id])
           (handler req)))))))

; Helpers ----------------------------------------

(provide/contract
 [make/smoke-servlet (-> servlet?
                         (-> exn? response/c)
                         (-> connection? request? any))]
 [make-smoke-servlet (->* ((-> response/c) path-string?)
                          (manager? custodian? namespace?)
                          servlet?)])
