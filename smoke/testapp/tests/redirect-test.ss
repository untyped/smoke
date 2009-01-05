#lang scheme/base

(require "../../test-base.ss")

; Tests ------------------------------------------

(define redirect-tests
  (test-suite "redirect"
    
    (test-case "full"
      (open/wait (controller-url redirect))
      (click/wait (node/link/text "Full refresh"))
      (check-equal? (title-ref) "Smoke test application"))
    
    (test-case "ajax"
      (open/wait (controller-url redirect))
      (click/wait (node/link/text "AJAX refresh"))
      (sleep 1) ; AJAX redirects are difficult to wait for as they involve two HTTP round-trips
      (check-equal? (title-ref) "Smoke test application"))))

; Provide statements -----------------------------

(provide redirect-tests)
