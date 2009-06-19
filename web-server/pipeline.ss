#lang scheme/base

(require net/url
         scheme/contract
         web-server/http/request-structs
         web-server/http/response-structs
         web-server/servlet-env
         (planet untyped/delirium:3)
         (planet untyped/mirrors:2)
         "../base.ss"
         "resume.ss"
         "send-suspend-dispatch.ss"
         "session.ss"
         "smoke-lru.ss")

(define-struct init-smoke-stage (proc)
  #:transparent
  #:property prop:procedure (struct-field-index proc))

(define establish-session-stage
  (make-init-smoke-stage
   (lambda (continue . args)
     ; any ... -> any
     ; Make sure we're passing the current request on (if any).
     (define (*continue*)
       (if (and (pair? args) (request? (car args)))
           (apply continue (current-request) (cdr args))
           (apply continue args)))
     (if (request-session (current-request))
         (*continue*)
         (start-session #f (lambda (session) (*continue*)))))))

(define establish-prompt-stage
  (make-init-smoke-stage
   (lambda (continue . args)
     ; -> any
     ; Eliminate the request argument.
     (define (*continue*)
       (if (and (pair? args) (request? (car args)))
           (apply continue (cdr args))
           (apply continue args)))
     (if (resume-available?)
         (*continue*)
         ;(parameterize ([current-frame (push-frame)])
           (send/suspend/dispatch
            (lambda (embed-url)
              (let* ([url0 (request-uri (current-request))]
                     [url1 (string->url (embed-url *continue*))])
                (make-redirect-response (make-url (url-scheme url1)
                                                  (url-user url1)
                                                  (url-host url1)
                                                  (url-port url1)
                                                  (url-path-absolute? url1)
                                                  (url-path url1)
                                                  (url-query url0)
                                                  (url-fragment url0))))))
           ;)
           ))))

; pipeline
(define init-smoke-pipeline
  (list establish-session-stage
        establish-prompt-stage))

; Provide statements -----------------------------

(provide/contract
 [init-smoke-stage?       (-> any/c boolean?)]
 [establish-session-stage procedure?]
 [establish-prompt-stage  procedure?]
 [init-smoke-pipeline     (listof procedure?)])
