#lang scheme

(require scheme/contract
         web-server/servlet)

; Utility procedures -----------------------------

; any -> boolean
(define (ajax-request? request)
  (and (request? request)
       ; The requested-with clause should match all AJAX requests sent by JQuery,
       ; but we include the page-id clause anyway for good measure:
       (or (ajax-request-requested-with request)
           (ajax-request-page-id request))
       #t))

; request -> (U string #f)
(define (ajax-request-requested-with request)
  ; (U string #f)
  (ormap (lambda (pair)
           (and (or (equal? (car pair) 'x-requested-with)
                    (equal? (car pair) 'X-Requested-With))
                (cdr pair)))
         (request-headers request)))

; request -> (U symbol #f)
(define (ajax-request-page-id request)
  ; (U symbol #f)
  (ormap (lambda (pair)
           (and (or (equal? (car pair) 'x-smoke-page)
                    (equal? (car pair) 'X-Smoke-Page))
                (string->symbol (cdr pair))))
         (request-headers request)))

; Provide statements -----------------------------

(provide/contract
 [ajax-request?               (-> any/c boolean?)]
 [ajax-request-requested-with (-> request? (or/c string? #f))]
 [ajax-request-page-id        (-> request? (or/c symbol? #f))])
