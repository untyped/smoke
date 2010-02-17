#lang scheme

(require "../../test-base.ss")

; Tests ------------------------------------------

(define-check (check-page-and-site-defined)
  (check-pred (cut regexp-match #rx"^#" <>) (inner-html-ref (node/id 'current-site)))
  (check-pred (cut regexp-match #rx"^#" <>) (inner-html-ref (node/id 'current-page))))

(define/provide-test-suite send-tests
  
  (test-case "send/suspend"
    (open/wait (controller-url send-page))
    (check-page-and-site-defined)
    (click/wait (node/id 'send-suspend))
    (check-page-and-site-defined)
    (click/wait (node/id 'continue))
    (check-page-and-site-defined))
  
  (test-case "send/suspend/dispatch"
    (open/wait (controller-url send-page))
    (check-page-and-site-defined)
    (click/wait (node/id 'send-suspend-dispatch))
    (check-page-and-site-defined)
    (click/wait (node/id 'continue))
    (check-page-and-site-defined)))
  
  