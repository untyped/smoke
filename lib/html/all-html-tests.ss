#lang scheme/base

(require "../../test-base.ss"
         "date-field-test.ss")

; Tests ------------------------------------------

(define/provide-test-suite all-html-tests
  date-field-tests)
