#lang scheme/base

(require "../../test-base.ss"
         "../base.ss")

; Tests ------------------------------------------

(define redirect-tests
  (test-suite "redirect"
    
    (test-case "full"
      (open/wait (controller-url redirect-page))
      (click/wait (node/link/text "Full refresh"))
      (check-equal? (title-ref) "Smoke test site"))
    
    (test-case "ajax"
      (open/wait (controller-url redirect-page))
      (click/wait (node/link/text "AJAX refresh"))
      (sleep 1) ; AJAX redirects are difficult to wait for as they involve two HTTP round-trips
      (check-equal? (title-ref) "Smoke test site"))))

; Provide statements -----------------------------

(provide redirect-tests)
