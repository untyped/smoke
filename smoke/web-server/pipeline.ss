#lang scheme/base

(require net/url
         scheme/contract
         web-server/http/request-structs
         web-server/http/response-structs
         web-server/servlet-env
         (planet untyped/delirium:2)
         (planet untyped/mirrors:1)
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
     ; Make sure we're passing the current request on (if any).
     (define (*continue*)
       (if (and (pair? args) (request? (car args)))
           (apply continue (current-request) (cdr args))
           (apply continue args)))
     (if (resume-available?)
         (*continue*)
         (begin
           (parameterize ([current-frame (push-frame)])
             (send/suspend/dispatch
              (lambda (embed-url)
                (make-redirect-response
                 (embed-url *continue*))))))))))

; pipeline
(define init-smoke-pipeline
  (list establish-session-stage establish-prompt-stage))

; Provide statements -----------------------------

(provide/contract
 [init-smoke-stage?       (-> any/c boolean?)]
 [establish-session-stage procedure?]
 [establish-prompt-stage  procedure?]
 [init-smoke-pipeline     (listof procedure?)])
