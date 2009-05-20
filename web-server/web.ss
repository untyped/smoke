#lang scheme/base

(require net/url
         scheme/contract
         scheme/match
         web-server/managers/manager
         web-server/http/response-structs
         web-server/http/request-structs
         web-server/private/servlet
         web-server/private/util
         web-server/servlet/servlet-structs
         (prefix-in ws: web-server/servlet/web)
         (planet untyped/mirrors:1)
         "continuation-url.ss")

; (parameter (U (request -> response) #f))
#;(define current-servlet-continuation-expiration-handler
  (make-parameter #f))

; -> void
#;(define (clear-continuation-table!)
  ((manager-clear-continuations! (current-servlet-manager)) (current-servlet-instance-id)))

; response -> void
#;(define (send/back resp)
  (abort-current-continuation servlet-prompt (lambda () resp)))

; (url -> response) [(request -> response)] -> request
#;(define (send/suspend response-generator
                      [expiration-handler (current-servlet-continuation-expiration-handler)])
  #;(define wcs (capture-web-cell-set))
  (begin0
    (call-with-composable-continuation
     (lambda (k)
       (define instance-id (current-servlet-instance-id))
       (define ctxt (current-execution-context))
       (define k-embedding ((manager-continuation-store! (current-servlet-manager))
                            instance-id
                            (make-custodian-box (current-custodian) k)
                            expiration-handler))
       (define k-url (url->string (url->continuation-url
                                   (request-uri (execution-context-request ctxt))
                                   (list* instance-id k-embedding))))
       (send/back (response-generator k-url)))
     servlet-prompt)
    #;(restore-web-cell-set! wcs)))

; ((proc -> url) -> response) [(request -> response)] -> request
#;(define (send/suspend/dispatch response-generator)
  ; This restores the tail position.
  ; Note: Herman's syntactic strategy would fail without the new-request capture.
  ;       (Moving this to the tail-position is not possible anyway, by the way.)
  (let ([thunk 
         (call-with-current-continuation
          (lambda (k0)
            (send/back
             (response-generator
              (lambda (proc [expiration-handler (current-servlet-continuation-expiration-handler)])
                (let/ec k1 
                  ; This makes the second continuation captured by send/suspend smaller
                  (call-with-continuation-prompt
                   (lambda ()
                     (let ([new-request (send/suspend k1 expiration-handler)])
                       (k0 (lambda () (proc new-request)))))
                   servlet-prompt))))))
          servlet-prompt)])
    (thunk)))

; Provide statements -----------------------------

#;(define web-server-embed/url?
  (->* ((-> request? any/c)) (expiration-handler/c) string?))

#;(provide/contract
 [current-servlet-continuation-expiration-handler parameter?]
 [clear-continuation-table! (-> void?)]
 [send/suspend/dispatch     (-> (-> web-server-embed/url? (or/c response/full? response/incremental?)) any/c)])

(provide (rename-out [ws:current-servlet-continuation-expiration-handler current-servlet-continuation-expiration-handler]
                     [ws:clear-continuation-table!                       clear-continuation-table!]
                     [ws:send/suspend/dispatch                           send/suspend/dispatch]))