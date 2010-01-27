#lang web-server

(require srfi/13
         (planet untyped/unlib:3/symbol)
         "../../test-base.ss")

; Helpers ----------------------------------------


; Tests ------------------------------------------

(define editor-tests
  (test-suite "editor"
    
    (test-case "test-page displays"
      (open/wait "/editor")
      (check-equal? (title-ref) "Editor")
      (check-equal? (inner-html-ref (node/id 'total-commits)) "0"))
    
    (test-case "correct values allow form submission"
      (enter-text (node/id 'larger-field) "2")
      (enter-text (node/id 'smaller-field) "1")
      (for ([i (in-range 1 4)])
        (click/wait (node/id 'submit-button))
        (check-equal? (title-ref) "Editor")
        (check-equal? (inner-html-ref (node/id 'total-commits)) (number->string i))))
    
    (test-case "failures prevent form submission"
      (enter-text (node/id 'larger-field) "1")
      (enter-text (node/id 'smaller-field) "2")
      (for ([i (in-range 1 4)])
        (click/wait (node/id 'submit-button))
        (check-equal? (title-ref) "Editor")
        (check-true (node-exists? (node/jquery "#larger-field-wrapper img.check-failure")))
        (check-true (node-exists? (node/jquery "#smaller-field-wrapper img.check-failure")))
        (check-equal? (inner-html-ref (node/id 'total-commits)) "3")))
    
    (test-case "warnings require confirmation"
      (for ([i (in-range 3 6)])
        (enter-text (node/id 'larger-field) (number->string i))
        (enter-text (node/id 'smaller-field) (number->string i))
        (click/wait (node/id 'submit-button))
        (check-equal? (title-ref) "Editor")
        (check-true (node-exists? (node/jquery "#larger-field-wrapper img.check-warning")))
        (check-true (node-exists? (node/jquery "#smaller-field-wrapper img.check-warning")))
        (check-equal? (inner-html-ref (node/id 'total-commits)) (number->string i))
        (click/wait (node/id 'submit-button))
        (check-equal? (title-ref) "Editor")
        (check-true (node-exists? (node/jquery "#larger-field-wrapper img.check-warning")))
        (check-true (node-exists? (node/jquery "#smaller-field-wrapper img.check-warning")))
        (check-equal? (inner-html-ref (node/id 'total-commits)) (number->string (add1 i)))))
   
    (test-case "changing field values without removing warnings never allows submission"
      (for ([i (in-range 6 10)])
        (enter-text (node/id 'larger-field) (number->string i))
        (enter-text (node/id 'smaller-field) (number->string i))
        (click/wait (node/id 'submit-button))
        (check-equal? (title-ref) "Editor")
        (check-true (node-exists? (node/jquery "#larger-field-wrapper img.check-warning")))
        (check-true (node-exists? (node/jquery "#smaller-field-wrapper img.check-warning")))
        (check-equal? (inner-html-ref (node/id 'total-commits)) "6")))))

; Provide statements -----------------------------

(provide editor-tests)

