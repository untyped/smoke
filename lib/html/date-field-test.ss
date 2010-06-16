#lang scheme/base

(require "../../test-base.ss")

(require (prefix-in srfi- srfi/19)
         (planet untyped/unlib:3/date)
         "date-field.ss")

; Tests ------------------------------------------

(define/provide-test-suite date-field-tests
  
  (test-case "set-value!, get-value : time zones"
    (let ([field (new date-field%)])
      (parameterize ([current-tz "GB"])
        (send field set-value! (srfi-make-date 0 00 00 00 28 03 2010 0))
        (check-equal? (send field get-value) (srfi-make-date 0 00 00 00 28 03 2010 0))
        (check-equal? (time-utc->date (send field get-time-utc)) (srfi-make-date 0 00 00 00 28 03 2010 0))
        (check-equal? (time-tai->date (send field get-time-tai)) (srfi-make-date 0 00 00 00 28 03 2010 0))
        (send field set-value! (srfi-make-date 0 00 00 01 28 03 2010 0))
        (check-equal? (send field get-value) (srfi-make-date 0 00 00 02 28 03 2010 3600))
        (check-equal? (time-utc->date (send field get-time-utc)) (srfi-make-date 0 00 00 02 28 03 2010 3600))
        (check-equal? (time-tai->date (send field get-time-tai)) (srfi-make-date 0 00 00 02 28 03 2010 3600))
        (send field set-tz! "PST8PDT")
        (send field set-value! (srfi-make-date 0 00 00 00 28 03 2010 0))
        ; Offset -25200 is PDT (summer time) because PST changes to PDT on 14/03/2010:
        (check-equal? (send field get-value) (srfi-make-date 0 00 00 17 27 03 2010 -25200))
        ; These figures are converted to dates in this module (i.e. in GMT):
        (check-equal? (time-utc->date (send field get-time-utc)) (srfi-make-date 0 00 00 00 28 03 2010 0))
        (check-equal? (time-tai->date (send field get-time-tai)) (srfi-make-date 0 00 00 00 28 03 2010 0))
        (send field set-value! (srfi-make-date 0 00 00 01 28 03 2010 0))
        ; Offset -25200 is PDT (summer time) because PST changes to PDT on 14/03/2010:
        (check-equal? (send field get-value) (srfi-make-date 0 00 00 18 27 03 2010 -25200))
        ; These figures are converted to dates in this module (i.e. in GMT):
        (check-equal? (time-utc->date (send field get-time-utc)) (srfi-make-date 0 00 00 02 28 03 2010 3600))
        (check-equal? (time-tai->date (send field get-time-tai)) (srfi-make-date 0 00 00 02 28 03 2010 3600)))))
  
  )