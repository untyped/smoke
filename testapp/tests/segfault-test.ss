#lang web-server

(require srfi/13/string
         "../../test-base.ss")

; Tests ------------------------------------------

(define segfault-tests
  (test-suite "segfault"
    
    (test-case "test-page displays"
      (open/wait "/segfault")
      (check-equal? (title-ref) "Segfault test"))
    
    (test-case "stress test"
        (open/wait "/segfault")
        (for ([i (in-range 0 10)])
          (with-check-info (['iteration i])
            (reload/wait)
            (click/wait (node/id 'submit-button)))))))

; Provide statements -----------------------------

(provide segfault-tests)

