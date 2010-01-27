#lang web-server

(require srfi/13/string
         "../../test-base.ss")

; Tests ------------------------------------------

(define tooltip-tests
  (test-suite "tooltip"
    
    (test-case "test-page displays"
      (open/wait "/tooltip")
      (check-equal? (title-ref) "Tooltip"))
    
    (test-case "run tests manually"
      (printf "Manual intervention required.~n")
      (printf "Follow the instructions on the test page and press ENTER when you are done:")
      (read-line))))

; Provide statements -----------------------------

(provide tooltip-tests)

