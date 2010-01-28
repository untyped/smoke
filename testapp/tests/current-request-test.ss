#lang scheme/base

(require srfi/13
         "../../test-base.ss"
         "../base.ss")

; Tests ------------------------------------------

(define current-request-tests
  (test-suite "current-request"
    
    (test-case "pipeline"
      (check-not-exn
        (lambda ()
          (for ([i (in-range 0 10)])
            (open/wait (controller-url test-current-request))
            (for ([j (in-range 0 10)])
              (printf "~a ~a~n" i j)
              (click/wait (node/id 'ajax)))))))))

; Provide statements -----------------------------

(provide current-request-tests)

