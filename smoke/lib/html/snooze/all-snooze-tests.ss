#lang scheme/base

(require "../../../test-base.ss"
         "report-test.ss")

; Tests ------------------------------------------

(define all-snooze-tests
  (test-suite "snooze"
    report-tests))

; Provide statements -----------------------------

(provide all-snooze-tests)
