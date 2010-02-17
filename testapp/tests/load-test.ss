#lang scheme/base

(require srfi/13/string
         "../../test-base.ss")

; Tests ------------------------------------------

(define load-tests
  (test-suite "load"
    
    (test-case "load test"
      (open/wait "/load")
      (for ([i (in-range 0 100)])
        (with-check-info (['iteration i])
          (enter-text/wait (node/id (format "field~a" i)) (number->string i)))))))

; Provide statements -----------------------------

(provide load-tests)

