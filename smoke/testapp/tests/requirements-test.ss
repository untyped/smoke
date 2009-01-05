#lang scheme/base

(require "../../lib-base.ss"
         "../../test-base.ss")

; Helpers ----------------------------------------

(define-check (check-initial num)
  (open/wait "/requirements")
  (check-equal? (js-ref (js htmlReqsLoaded)) num)
  (check-equal? (js-ref (js jsReqsLoaded)) num))

(define-check (check-refresh id num)
  (click/wait (node/id id))
  (check-equal? (js-ref (js htmlReqsLoaded)) num)
  (check-equal? (js-ref (js jsReqsLoaded)) num))

; Tests ------------------------------------------

(define requirements-tests
  (test-suite "requirements"
    
    (test-case "ajax"
      (check-initial 1)
      (check-refresh 'ajax 2)
      (check-refresh 'ajax 2))
    
    (test-case "full"
      (check-initial 1)
      (check-refresh 'full 2)
      (check-refresh 'full 2))
    
    (test-case "mixed"
      (check-initial 1)
      (check-refresh 'ajax 2)
      (check-refresh 'full 2))))

; Provide statements -----------------------------

(provide requirements-tests)

