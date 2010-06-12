#lang scheme/base

(require "smoke.ss"
         "test-base.ss"
         "class/all-class-tests.ss"
         "lib/html/all-html-tests.ss"
         #;"resource/all-resource-tests.ss"
         "web-server/all-web-server-tests.ss")

; Tests ------------------------------------------

(define/provide-test-suite all-smoke-tests
  all-web-server-tests
  all-class-tests
  all-html-tests
  #;all-resource-tests)
