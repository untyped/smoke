#lang scheme/base

(require "smoke.ss"
         "test-base.ss"
         "class/all-class-tests.ss"
         #;"resource/all-resource-tests.ss"
         "testapp/tests/all-testapp-tests.ss"
         #;"web-server/all-web-server-tests.ss")

; Tests ------------------------------------------

(define all-smoke-tests
  (test-suite "smoke"
    #;all-web-server-tests
    all-class-tests
    ;all-resource-tests
    all-testapp-tests))

; Provide statements -----------------------------

(provide all-smoke-tests)
