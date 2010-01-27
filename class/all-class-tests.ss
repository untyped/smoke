#lang scheme

(require "../test-base.ss"
         "class-internal-test.ss"
         "syntax-test.ss")

; Tests ------------------------------------------

(define all-class-tests
  (test-suite "class"
    class-internal-tests
    syntax-tests))

; Provide statements -----------------------------

(provide all-class-tests)
