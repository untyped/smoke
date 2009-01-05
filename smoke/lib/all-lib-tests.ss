#lang scheme/base

(require "../test-base.ss"
         "component-test.ss"
         "html/all-html-tests.ss")

; Tests ------------------------------------------

(define all-lib-tests
  (test-suite "lib"
    component-tests
    all-html-tests))

; Provide statements -----------------------------

(provide all-lib-tests)
