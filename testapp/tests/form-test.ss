#lang scheme/base

(require srfi/13
         (planet untyped/unlib:3/symbol)
         "../../test-base.ss")

; Helpers ----------------------------------------

; symbol -> any
(define (field-value id)
  (js-ref (!dot (!index ,(node/id id) 0) value)))

; symbol symbol -> any
(define (field-attr id attr-id)
  (js-ref (!index (!index ,(node/id id) 0) ,attr-id)))

; symbol -> any
(define (printed-value id)
  (inner-html-ref (node/id (symbol-append id '-value))))

; symbol any -> void
(define-check (check-field-value id expected)
  (check-equal? (field-value id) expected))

; symbol any -> void
(define-check (check-field-attr id attr-id expected)
  (check-equal? (field-attr id attr-id) expected))

; symbol any -> void
(define-check (check-printed-value id expected)
  (check-equal? (printed-value id) (format "~s" expected)))

; symbol any -> void
(define-check (check-combo-value id expected)
  (check-equal?
   (js-ref ((function ()
              (var [elem (!index ,(node/id id) 0)])
              (return (!dot (!index (!dot elem options) (!dot elem selectedIndex)) text)))))
   expected))

; Tests ------------------------------------------

(define form-tests
  (test-suite "form"
    
    (test-case "test-page displays"
      (open/wait "/form")
      (check-equal? (title-ref) "Form elements"))
    
    ; Have to do password field first as submitting the page clears its value:
    
    (test-suite "password-field"
      
      (test-case "initial value"
        (check-field-attr 'password-field 'size 20)
        (check-field-attr 'password-field 'maxLength 10)
        (check-field-value 'password-field "")
        (check-printed-value 'password-field "Initial"))
      
      (test-case "full refresh"
        (enter-text (node/id 'password-field) "Full")
        (click/wait (node/id 'submit-button))
        (check-field-value 'password-field "")
        (check-printed-value 'password-field "Full"))
      
      (test-case "ajax refresh"
        (enter-text (node/id 'password-field) "AJAX")
        (click/wait (node/id 'ajax-submit-button))
        (check-field-value 'password-field "")
        (check-printed-value 'password-field "AJAX"))
      
      (test-case "blank"
        (enter-text (node/id 'password-field) "")
        (click/wait (node/id 'submit-button))
        (check-printed-value 'password-field #f))
      
      (test-case "value too long"
        (enter-text (node/id 'password-field) "abcdefghijk")
        (click/wait (node/id 'submit-button))
        (check-printed-value 'password-field "abcdefghij"))
      
      (test-case "value trimmable"
        (enter-text (node/id 'password-field) "   abc   ")
        (click/wait (node/id 'submit-button))
        (check-printed-value 'password-field "abc")
        (enter-text (node/id 'password-field) "Done")))
    
    (test-suite "text-field"
      
      (test-case "initial value"
        (check-field-attr 'text-field 'size 20)
        (check-field-attr 'text-field 'maxLength 10)
        (check-field-value 'text-field "Initial")
        (check-printed-value 'text-field "Initial"))
      
      (test-case "full refresh"
        (enter-text (node/id 'text-field) "Full")
        (click/wait (node/id 'submit-button))
        (check-field-value 'text-field "Full")
        (check-printed-value 'text-field "Full"))
      
      (test-case "ajax refresh"
        (enter-text (node/id 'text-field) "AJAX")
        (click/wait (node/id 'ajax-submit-button))
        (check-field-value 'text-field "AJAX")
        (check-printed-value 'text-field "AJAX"))
      
      (test-case "blank"
        (enter-text (node/id 'text-field) "")
        (click/wait (node/id 'submit-button))
        (check-printed-value 'text-field #f))
      
      (test-case "value too long"
        (enter-text (node/id 'text-field) "abcdefghijk")
        (click/wait (node/id 'submit-button))
        (check-printed-value 'text-field "abcdefghij"))
      
      (test-case "value trimmable"
        (enter-text (node/id 'text-field) "   abc   ")
        (click/wait (node/id 'submit-button))
        (check-printed-value 'text-field "abc")
        (enter-text (node/id 'text-field) "Done"))
      
      (test-case "uppercase case conversion"
        (enter-text (node/id 'uppercase-text-field) "   abc   ")
        (click/wait (node/id 'submit-button))
        (check-printed-value 'uppercase-text-field "ABC")
        (enter-text (node/id 'uppercase-text-field) "Done"))
      
      (test-case "lowercase case conversion"
        (enter-text (node/id 'lowercase-text-field) "   ABC   ")
        (click/wait (node/id 'submit-button))
        (check-printed-value 'lowercase-text-field "abc")
        (enter-text (node/id 'lowercase-text-field) "Done"))
      
      (test-case "placeholder"
        (for ([button-id (in-list '(submit-button ajax-submit-button))])
          (with-check-info (['button-id button-id])
            (check-field-value 'placeholder-text-field "holder")
            (check-printed-value 'placeholder-text-field #f)
            (focus (node/id 'placeholder-text-field))
            (enter-text (node/id 'placeholder-text-field) "holder")
            (blur (node/id 'placeholder-text-field))
            (click/wait (node/id button-id))
            (check-field-value 'placeholder-text-field "holder")
            (check-printed-value 'placeholder-text-field "holder")
            (focus (node/id 'placeholder-text-field))
            (enter-text (node/id 'placeholder-text-field) "")
            (blur (node/id 'placeholder-text-field))
            (click/wait (node/id button-id))))))
    
    (test-suite "text-area"
      
      (test-case "initial value"
        (check-field-attr 'text-area 'cols 20)
        (check-field-attr 'text-area 'rows 10)
        (check-field-value 'text-area "Initial")
        (check-printed-value 'text-area "Initial"))
      
      (test-case "full refresh"
        (enter-text (node/id 'text-area) "Full")
        (click/wait (node/id 'submit-button))
        (check-field-value 'text-area "Full")
        (check-printed-value 'text-area "Full"))
      
      (test-case "ajax refresh"
        (enter-text (node/id 'text-area) "AJAX")
        (click/wait (node/id 'ajax-submit-button))
        (check-field-value 'text-area "AJAX")
        (check-printed-value 'text-area "AJAX"))
      
      (test-case "blank"
        (enter-text (node/id 'text-area) "")
        (click/wait (node/id 'submit-button))
        (check-printed-value 'text-area #f))
      
      (test-case "multi line"
        (enter-text (node/id 'text-area) "abcde\nfghij")
        (click/wait (node/id 'submit-button))
        (check-printed-value 'text-area "abcde\r\nfghij"))
      
      (test-case "value trimmable"
        (enter-text (node/id 'text-area) "   abc   ")
        (click/wait (node/id 'submit-button))
        (check-printed-value 'text-area "abc")
        (enter-text (node/id 'text-area) "Done"))
      
      (test-case "uppercase case conversion"
        (enter-text (node/id 'uppercase-text-area) "   abc   ")
        (click/wait (node/id 'submit-button))
        (check-printed-value 'uppercase-text-area "ABC")
        (enter-text (node/id 'uppercase-text-area) "Done"))
      
      (test-case "lowercase case conversion"
        (enter-text (node/id 'lowercase-text-area) "   ABC   ")
        (click/wait (node/id 'submit-button))
        (check-printed-value 'lowercase-text-area "abc")
        (enter-text (node/id 'lowercase-text-area) "Done"))
      
      (test-case "placeholder"
        (for ([button-id (in-list '(submit-button ajax-submit-button))])
          (with-check-info (['button-id button-id])
            (check-field-value 'placeholder-text-area "holder")
            (check-printed-value 'placeholder-text-area #f)
            (focus (node/id 'placeholder-text-area))
            (enter-text (node/id 'placeholder-text-area) "holder")
            (blur (node/id 'placeholder-text-area))
            (click/wait (node/id button-id))
            (check-field-value 'placeholder-text-area "holder")
            (check-printed-value 'placeholder-text-area "holder")
            (focus (node/id 'placeholder-text-area))
            (enter-text (node/id 'placeholder-text-area) "")
            (blur (node/id 'placeholder-text-area))
            (click/wait (node/id button-id))))))
    
    (test-suite "tiny-mce"
      
      ;(test-case "initial value"
      ;  (check-field-attr 'text-area 'cols 20)
      ;  (check-field-attr 'text-area 'rows 10)
      ;  (check-field-value 'text-area "<p>Initial</p>")
      ;  (check-printed-value 'text-area "<p>Initial</p>"))
      
      (test-case "full refresh"
        (printf "These tests have not been automated yet (might need an exec-javascript Delirium command).")
        (printf "Change the value of the tinyMCE field, do a full refresh, and check the reported value changes:")
        (read-line))
      
      (test-case "full refresh"
        (printf "Change the value of the tinyMCE field, do an AJAX refresh, and check the reported value changes:")
        (read-line)))
    
    (test-suite "check-box"
      
      (test-case "initial value"
        (check-field-attr 'check-box 'checked #t)
        (check-printed-value 'check-box #t))
      
      (test-case "full refresh"
        (click (node/id 'check-box))
        (click/wait (node/id 'submit-button))
        (check-field-attr 'check-box 'checked #f)
        (check-printed-value 'check-box #f)
        (click (node/id 'check-box))
        (click/wait (node/id 'submit-button))
        (check-field-attr 'check-box 'checked #t)
        (check-printed-value 'check-box #t))
      
      (test-case "ajax refresh"
        (click (node/id 'check-box))
        (click/wait (node/id 'ajax-submit-button))
        (check-field-attr 'check-box 'checked #f)
        (check-printed-value 'check-box #f)
        (click (node/id 'check-box))
        (click/wait (node/id 'ajax-submit-button))
        (check-field-attr 'check-box 'checked #t)
        (check-printed-value 'check-box #t)))
    
    (test-suite "combo-box"
      
      (test-case "initial value"
        (check-field-value 'combo-box "1")
        (check-combo-value 'combo-box "Option 1")
        (check-printed-value 'combo-box 1))
      
      (test-case "full"
        (select (node/id 'combo-box) "2")
        (click/wait (node/id 'submit-button))
        (check-field-value 'combo-box "2")
        (check-combo-value 'combo-box "Option 2")
        (check-printed-value 'combo-box 2))
      
      (test-case "ajax"
        (select (node/id 'combo-box) "1")
        (click/wait (node/id 'ajax-submit-button))
        (check-field-value 'combo-box "1")
        (check-combo-value 'combo-box "Option 1")
        (check-printed-value 'combo-box 1))
      
      (test-case "symbol"
        (select (node/id 'combo-box) "b")
        (click/wait (node/id 'ajax-submit-button))
        (check-field-value 'combo-box "b")
        (check-combo-value 'combo-box "Option 4")
        (check-printed-value 'combo-box 'b))
      
      (test-case "boolean"
        (select (node/id 'combo-box) "--yes--")
        (click/wait (node/id 'ajax-submit-button))
        (check-field-value 'combo-box "--yes--")
        (check-combo-value 'combo-box "Option 5")
        (check-printed-value 'combo-box #t)))
    
    (test-suite "radio-combo"
      
      (test-suite "horizontal"
        
        (test-case "initial value"
          (check-field-attr 'radio-combo-h-1 'checked #t)
          (check-field-attr 'radio-combo-h-a 'checked #f)
          (check-field-attr 'radio-combo-h---yes-- 'checked #f)
          (check-printed-value 'radio-combo-h 1))
        
        (test-case "full"
          (click (node/id 'radio-combo-h-a))
          (click/wait (node/id 'submit-button))
          (check-field-attr 'radio-combo-h-1 'checked #f)
          (check-field-attr 'radio-combo-h-a 'checked #t)
          (check-field-attr 'radio-combo-h---yes-- 'checked #f)
          (check-printed-value 'radio-combo-h 'a))
        
        (test-case "ajax"
          (click (node/id 'radio-combo-h---yes--))
          (click/wait (node/id 'ajax-submit-button))
          (check-field-attr 'radio-combo-h-1 'checked #f)
          (check-field-attr 'radio-combo-h-a 'checked #f)
          (check-field-attr 'radio-combo-h---yes-- 'checked #t)
          (check-printed-value 'radio-combo-h #t)))
      
      (test-suite "vertical"
        
        (test-case "initial value"
          (check-field-attr 'radio-combo-v-1 'checked #t)
          (check-field-attr 'radio-combo-v-a 'checked #f)
          (check-field-attr 'radio-combo-v---yes-- 'checked #f)
          (check-printed-value 'radio-combo-v 1))
        
        (test-case "full"
          (click (node/id 'radio-combo-v-a))
          (click/wait (node/id 'submit-button))
          (check-field-attr 'radio-combo-v-1 'checked #f)
          (check-field-attr 'radio-combo-v-a 'checked #t)
          (check-field-attr 'radio-combo-v---yes-- 'checked #f)
          (check-printed-value 'radio-combo-v 'a))
        
        (test-case "ajax"
          (click (node/id 'radio-combo-v---yes--))
          (click/wait (node/id 'ajax-submit-button))
          (check-field-attr 'radio-combo-v-1 'checked #f)
          (check-field-attr 'radio-combo-v-a 'checked #f)
          (check-field-attr 'radio-combo-v---yes-- 'checked #t)
          (check-printed-value 'radio-combo-v #t))))
    
    (test-suite "radio-button"
      
      (test-case "initial"
        (check-printed-value 'radio-group 'radio2))
      
      (test-case "full"
        (click (node/id 'radio-button1))
        (click/wait (node/id 'submit-button))
        (check-printed-value 'radio-group 'radio1))
      
      (test-case "ajax"
        (click (node/id 'radio-button3))
        (click/wait (node/id 'ajax-submit-button))
        (check-printed-value 'radio-group 'radio3)))))

; Provide statements -----------------------------

(provide form-tests)

