#lang scheme/base

(require scheme/contract
         web-server/servlet)

; Utility procedures -----------------------------

; any -> boolean
(define (ajax-request? request)
  (and (request? request)
       (ajax-request-page-id request)
       #t))

; request -> (U symbol #f)
(define (ajax-request-page-id request)
  ; (U symbol #f)
  ; TODO : Add cast from bytes to symbol:
  (ormap (lambda (pair)
           (and (or (equal? (car pair) 'x-smoke-page)
                    (equal? (car pair) 'X-Smoke-Page))
                (string->symbol (cdr pair))))
         (request-headers request)))

; request -> boolean
(define (request-redirected-from-expired-instance? request)
  (ormap (lambda (pair)
           (or (equal? (car pair) 'x-smoke-expired-instance)
               (equal? (car pair) 'X-Smoke-Expired-Instance)))
         (request-headers request)))

; request -> boolean
(define (request-redirected-from-expired-continuation? request)
  (ormap (lambda (pair)
           (or (equal? (car pair) 'x-smoke-expired-continuation)
               (equal? (car pair) 'X-Smoke-Expired-Continuation)))
         (request-headers request)))

; Provide statements -----------------------------

(provide/contract
 [ajax-request?        (-> any/c boolean?)]
 [ajax-request-page-id (-> request? (or/c symbol? false/c))]
 [request-redirected-from-expired-instance?     (-> request? boolean?)]
 [request-redirected-from-expired-continuation? (-> request? boolean?)])
