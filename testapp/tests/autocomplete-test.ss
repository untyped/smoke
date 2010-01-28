#lang scheme/base

(require srfi/13/string
         "../../test-base.ss")

; Tests ------------------------------------------

(define autocomplete-tests
  (test-suite "autocomplete"
    
    (test-case "test-page displays"
      (open/wait "/autocomplete")
      (check-equal? (title-ref) "Autocomplete"))
    
    (test-case "run tests manually"
      (printf "Manual intervention required.~n")
      (printf "Follow the instructions on the test page and press ENTER when you are done:")
      (read-line))))

; Provide statements -----------------------------

(provide autocomplete-tests)

