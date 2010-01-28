#lang scheme/base

(require srfi/13
         (planet untyped/unlib:3/symbol)
         "../../lib-base.ss"
         "../../test-base.ss"
         "../page.ss"
         "html-component.ss"
         "html-page.ss")

; Tests ------------------------------------------

(define form-element-tests
  (test-suite "form-element.ss"
    
    (test-case "test-page displays"
      (open/page test-page)
      (check-equal? (title-ref) "Form elements"))
    
    ; Have to do password field first as submitting the page clears its value:
    
    (test-case "password-field"
      (let ([do-checks ; string string string -> void
             (lambda (name typed expected)
               (lambda (name typed expected)
                 (with-check-info (['sequence-name name])
                   (when typed
                     (enter-text (node/id 'password-field) typed)
                     (click/wait (node/id 'submit-button)))
                   (check-equal? (js-ref (!dot (!index ,(node/id 'password-field) 0) value)) #f)
                   (check-equal? (inner-html-ref (node/id 'password-field-value)) expected))))])
        (check-equal? (js-ref (!dot (!index ,(node/id 'password-field) 0) size)) 20)
        (check-equal? (js-ref (!dot (!index ,(node/id 'password-field) 0) maxLength)) 10)
        (do-checks "Initial" #f "Initial")
        (do-checks "Normal value" "Text" "Text")
        (do-checks "Blank" "" "no")
        (do-checks "Too long" "abcdefghijk" "abcdefghij")
        (do-checks "Trimmable" "   abc   " "abc")
        (enter-text (node/id 'password-field) "Done")))
    
    (test-case "text-field"
      (let ([do-checks ; string string string -> void
             (lambda (name typed expected)
               (with-check-info (['sequence-name name])
                 (when typed
                   (enter-text (node/id 'text-field) typed)
                   (click/wait (node/id 'submit-button)))
                 (check-equal? (js-ref (!dot (!index ,(node/id 'text-field) 0) value)) 
                               (or (and typed
                                        (if (> (string-length typed) 10)
                                            (string-take typed 10)
                                            typed))
                                   expected))
                 (check-equal? (inner-html-ref (node/id 'text-field-value)) expected)))])
        (check-equal? (js-ref (!dot (!index ,(node/id 'text-field) 0) size)) 20)
        (check-equal? (js-ref (!dot (!index ,(node/id 'text-field) 0) maxLength)) 10)
        (do-checks "Initial" #f "Initial")
        (do-checks "Normal value" "Text" "Text")
        (do-checks "Blank" "" "no")
        (do-checks "Too long" "abcdefghijk" "abcdefghij")
        (do-checks "Trimmable" "   abc   " "abc")
        (enter-text (node/id 'text-field) "Done")))
    
    (test-case "text-area"
      (let ([do-checks ; string (U string #f) string -> void
             (lambda (name typed expected)
               (with-check-info (['sequence-name name])
                 (when typed
                   (enter-text (node/id 'text-area) typed)
                   (click/wait (node/id 'submit-button)))
                 (check-equal? (js-ref (!dot (!index ,(node/id 'text-area) 0) value))
                               (or typed expected))
                 (check-equal? (inner-html-ref (node/id 'text-area-value)) expected)))])
        (check-equal? (js-ref (!dot (!index ,(node/id 'text-area) 0) rows)) 10)
        (check-equal? (js-ref (!dot (!index ,(node/id 'text-area) 0) cols)) 20)
        (do-checks "Initial" #f "Initial")
        (do-checks "Normal value" "Text" "Text")
        (do-checks "Blank" "" "no")
        (do-checks "Multi line" "Multi\nline" "Multi\nline")
        (do-checks "Trimmable" "   abc   " "abc")
        (enter-text (node/id 'text-area) "Done")))
    
    (test-case "check-box"
      (let ([do-checks ; string boolean -> void
             (lambda (name expected)
               (with-check-info (['sequence-name name])
                 (click (node/id 'check-box))
                 (click/wait (node/id 'submit-button))
                 (check-equal? (js-ref (!dot (!index ,(node/id 'check-box) 0) checked)) expected)
                 (check-equal? (inner-html-ref (node/id 'check-box-value)) (if expected "yes" "no"))))])
        (check-equal? (js-ref (!dot (!index ,(node/id 'check-box) 0) checked)) #t)
        (check-equal? (inner-html-ref (node/id 'check-box-value)) "yes")
        (do-checks "Uncheck" #f)
        (do-checks "Check" #t)))
    
    (test-case "combo-box"
      (let ([do-checks ; string (U string #f) string string string -> void
             (lambda (name selected expected expected-printed)
               (with-check-info (['sequence-name name])
                 (when selected
                   (select (node/id 'combo-box) selected)
                   (click/wait (node/id 'submit-button)))
                 (check-equal? (js-ref ((function ()
                                          (var [elem (!index ,(node/id 'combo-box) 0)])
                                          (return (!dot (!index (!dot elem options) (!dot elem selectedIndex)) text)))))
                               expected-printed)
                 (check-equal? (inner-html-ref (node/id 'combo-box-value)) expected)))])
        (do-checks "Initial" #f "b" "Option 4")
        (do-checks "Integer" "2" "2" "Option 2")
        (do-checks "Symbol" "b" "b" "Option 4")
        (do-checks "Boolean" "--yes--" "yes" "Option 5")))
    
    (test-case "radio-group and radio-button"
      (let ([do-checks ; string (U symbol #f) string -> void
             (lambda (name selected expected)
               (with-check-info (['sequence-name name])
                 (when selected
                   (click (node/id selected))
                   (click/wait (node/id 'submit-button))
                   (check-equal? (js-ref (!dot (!index ,(node/id selected) 0) checked)) #t))
                 (check-equal? (inner-html-ref (node/id 'radio-group-value)) expected)))])
        (check-equal? (js-ref (!dot (!index ,(node/id 'radio-button2) 0) checked)) #t)
        (do-checks "Initial" #f "radio2")
        (do-checks "Radio 1" 'radio-button1 "radio1")
        (do-checks "Radio 3" 'radio-button3 "radio3")))
    
    ))

; Helpers ----------------------------------------

(define radio-group (new radio-group% [id 'radio-group]))

(define radio-buttons
  (list (new radio-button% [id 'radio-button1] [group radio-group] [value 'radio1] [label "Radio 1"])
        (new radio-button% [id 'radio-button2] [group radio-group] [value 'radio2] [label "Radio 2"])
        (new radio-button% [id 'radio-button3] [group radio-group] [value 'radio3] [label "Radio 3"])))

(send radio-group set-value! 'radio2)

(define-singleton test-page html-page% ()
  
  ; Fields -------------------------------------
  
  ; (listof form-element%)
  (field controls
    `(,(new password-field% [id 'password-field] [value "Initial"] [size 20] [max-length 10])
      ,(new text-field% [id 'text-field] [value "Initial"] [size 20] [max-length 10])
      ,(new text-area% [id 'text-area] [value "Initial"] [rows 10] [cols 20])
      ,(new check-box% [id 'check-box] [value #t] [label "Label"])
      ,(new combo-box% [id 'combo-box] [value 'b] [options '((1  . "Option 1") 
                                                             (2  . "Option 2")
                                                             (a  . "Option 3")
                                                             (b  . "Option 4")
                                                             (#t . "Option 5")
                                                             (#f . "Option 6"))])
      ,(new file-field% [id 'file-field] [size 20])
      ,radio-group
      ,@radio-buttons
      ,(new submit-button% [id 'submit-button] [action (callback on-submit)] [label "Okay"]))
    #:children #:accessor #:mutator)
  
  ; Constructor --------------------------------
  
  (super-new [title "Form elements"])
  
  ; Methods ------------------------------------
  
  ; seed -> xml
  (define/override (render seed)
    (xml (table (tr (th "ID")
                    (th "Widget")
                    (th "Value"))
                ,@(map (lambda (control)
                         (xml (tr (th ,(send control get-id))
                                  (td ,(send control render seed))
                                  (td ,(if (is-a? control submit-button%)
                                           (xml)
                                           (xml (span (@ [id ,(symbol-append (send control get-id) '-value)])
                                                      ,(send control get-value))))))))
                       controls)))))

; Provide statements -----------------------------

(provide form-element-tests)

