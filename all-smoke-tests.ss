#lang scheme/base

(require "main.ss"
         "test-base.ss"
         "class/all-class-tests.ss"
         ;"resource/all-resource-tests.ss"
         "testapp/tests/all-testapp-tests.ss"
         "core/all-core-tests.ss")

; Tests ------------------------------------------

(define all-smoke-tests
  (test-suite "smoke"
    all-core-tests
    all-class-tests
    ;all-resource-tests
    all-testapp-tests))

; Provide statements -----------------------------

(provide all-smoke-tests)
