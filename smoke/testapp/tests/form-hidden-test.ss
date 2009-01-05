#lang scheme/base

(require srfi/13
         (planet untyped/unlib:3/symbol)
         "../../test-base.ss")

; Helpers ----------------------------------------

; symbol [#:tag string] -> test-suite
(define (make-input-tests id #:tag [tag "input"])
  (test-suite (symbol->string id)
    
    (test-case "initially hidden"
      (check-found (node/jquery (format "span#~a.smoke-hidden-component" id))))
    
    (test-case "click show"
      (click/wait (node/id (symbol-append id '-show-hide)))
      (check-found (node/jquery (format "~a#~a" tag id))))))

; Tests ------------------------------------------

(define form-tests/hidden
  (test-suite "form/hidden"
    
    (test-case "test-page displays"
      (open/wait "/form/hidden")
      (check-equal? (title-ref) "Form elements"))
    
    ; Have to do password field first as submitting the page clears its value:
    
    (make-input-tests 'password-field)
    (make-input-tests 'text-field)
    (make-input-tests 'text-area #:tag "textarea")
    (make-input-tests 'check-box)
    (make-input-tests 'combo-box #:tag "select")
    (make-input-tests 'radio-button1)
    (make-input-tests 'radio-button2)
    (make-input-tests 'radio-button3)))

; Provide statements -----------------------------

(provide form-tests/hidden)

