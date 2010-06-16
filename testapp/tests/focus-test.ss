#lang scheme/base

(require "../../lib-base.ss"
         "../../test-base.ss")

; Custom checks ----------------------------------

; Tests ------------------------------------------

(define focus-tests
  (test-suite "focus"
    
    (test-case "focus stays with field"
      (open/wait "/focus")
      
      ; It looks like the focus functionality works when interacting directly with the test app,
      ; but fails when interacting via Delirium. The "smoke-page-update" event doesn't seem to be
      ; getting fired when Delirium gets involved.
      
      (printf "Focus tests cannot be performed automatically.~n")
      (printf "Click on Field 1, enter a value, and press Tab.~n")
      (printf "Focus should transfer to Field 2 and a full stop should be appended to both fields.~n")
      (printf "Type \"y\" to confirm:~n")
      (let ([val (read-line)])
        (check-true (and (string? val) (string-ci=? val "y")))))))

; Provide statements -----------------------------

(provide focus-tests)

