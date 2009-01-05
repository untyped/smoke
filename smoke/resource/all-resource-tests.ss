#lang scheme

(require "../test-base.ss"
         "render-test.ss"
         "struct-test.ss")

; Tests -------------------------------------------

(define all-resource-tests
  (test-suite "resource"
    render-tests
    struct-tests))

; Provide statements -----------------------------

(provide all-resource-tests)
