#lang scheme/base

(require (planet untyped/unlib:3/list)
         (planet untyped/unlib:3/symbol)
         "../content-base.ss")

; Controllers ------------------------------------

; request -> response
(define-controller (form request)
  (send form-page respond))

; -> response
(define-controller (form/hidden request)
  (for ([control (send form-page get-controls)])
    (send control set-visible?! #f))
  (send form-page respond))

; Components -------------------------------------

(define radio-group (new radio-group% [id 'radio-group]))

(define radio-buttons
  (list (new radio-button% [id 'radio-button1] [group radio-group] [value 'radio1] [label "Radio 1"])
        (new radio-button% [id 'radio-button2] [group radio-group] [value 'radio2] [label "Radio 2"])
        (new radio-button% [id 'radio-button3] [group radio-group] [value 'radio3] [label "Radio 3"])))

(send radio-group set-value! 'radio2)

(define test-autocomplete-field%
  (class/cells autocomplete-field% ()
    
    (define/override (get-options prefix)
      (for/list ([suffix (in-string "abcde")])
        (format "~a~a" prefix suffix)))))

(define test-multi-column-autocomplete-field%
  (class/cells autocomplete-field% ()
    
    (super-new [multi-column? #t])
    
    (define/override (get-options prefix)
      (for/list ([number (in-naturals)]
                 [letter (in-string "abcde")]
                 [greek  (in-list (list "alpha" "beta" "gamma" "delta" "epsilon"))]
                 [latin  (in-list (list "i" "ii" "iii" "iv" "v"))])
        (list (format "Number ~a" number)
              (format "Letter ~a" letter)
              (format "Greek ~a" greek)
              (format "Latin ~a" latin))))))

(define form-page
  (singleton/cells (refreshable-mixin html-page%) ()
    
    (inherit refresh!)
    
    ; Fields -------------------------------------
    
    ; (listof form-element%)
    (field [controls `(,(new password-field% [id 'password-field] [value "Initial"] [size 20] [max-length 10])
                       ,(new text-field% [id 'text-field] [value "Initial"] [size 20] [max-length 10])
                       ,(new text-field% [id 'uppercase-text-field] [value "Initial"] [size 20] [max-length 10] [case-conversion 'uppercase])
                       ,(new text-field% [id 'lowercase-text-field] [value "Initial"] [size 20] [max-length 10] [case-conversion 'lowercase])
                       ,(new test-autocomplete-field%
                             [id         'autocomplete-field]
                             [value      "Initial"]
                             [size       20]
                             [max-length 10])
                       ,(new test-multi-column-autocomplete-field%
                             [id         'multi-column-autocomplete-field]
                             [value      "Initial"]
                             [size       20]
                             [max-length 10])
                       ,(new text-area% [id 'text-area] [value "Initial"] [rows 10] [cols 20])
                       ,(new text-area% [id 'uppercase-text-area] [value "Initial"] [rows 10] [cols 20] [case-conversion 'uppercase])
                       ,(new text-area% [id 'lowercase-text-area] [value "Initial"] [rows 10] [cols 20] [case-conversion 'lowercase])
                       ,(new tiny-mce% [id 'tiny-mce1] [value "Initial"] [rows 10] [cols 20])
                       ;,(new tiny-mce% [id 'tiny-mce2] [value "Initial"] [rows 10] [cols 20])
                       ,(new check-box% [id 'check-box] [value #t] [label "Label"])
                       ,(new combo-box% [id 'combo-box] [options '((1  . "Option 1") 
                                                                   (2  . "Option 2")
                                                                   (a  . "Option 3")
                                                                   (b  . "Option 4")
                                                                   (#t . "Option 5")
                                                                   (#f . "Option 6"))])
                       ,(new radio-combo% [id 'radio-combo-h] [vertical? #f] [options '((1 . "Option 1") (a . "Option 2") (#t . "Option 3"))])
                       ,(new radio-combo% [id 'radio-combo-v] [vertical? #t] [options '((1 . "Option 1") (a . "Option 2") (#t . "Option 3"))])
                       ,(new file-field% [id 'file-field] [size 20])
                       ,radio-group
                       ,@radio-buttons
                       ,(new multi-select% [id 'multi-select] [items '("Apples" "Oranges" "Pears")])
                       ,(new multi-select% [id 'multi-select2] [items null] [editor (new combo-box% [options '((a . "Value 1") (b . "Value 2") (c . "Value 3"))])])
                       ,(new submit-button% [id 'submit-button] [action (callback on-submit)] [label "OK (Full)"])
                       ,(new button% [id 'ajax-submit-button] [on-click (callback on-submit)] [label "OK (AJAX)"]))]
      #:children #:accessor #:mutator)
    
    ; Constructor --------------------------------
    
    (super-new [id 'form-test-page] [title "Form elements"])
    
    ; Methods ------------------------------------
    
    ; seed -> xml
    (define/augment (render seed)
      (xml (table (tr (th "ID")
                      (th "Widget")
                      (th "Value")
                      (th "Show/hide"))
                  ,@(for/list ([control controls])
                      (xml (tr (th ,(send control get-id))
                               (td ,(send control render seed))
                               (td ,(opt-xml (is-a? control form-element%)
                                      (pre (@ [id ,(symbol-append (send control get-id) '-value)]
                                              [style "padding: 0px; margin: 0px; font-family: monaco,courier"])
                                           ,(pretty-format (send control get-value)))))
                               (td (a (@ [id      ,(symbol-append (send control get-id) '-show-hide)]
                                         [onclick ,(embed/ajax seed (callback on-toggle-visibility (send control get-id)))])
                                      ,(if (send control get-visible?) "Hide" "Show")))))))))
    
    ; -> void
    (define/public #:callback (on-submit)
      (refresh!))
    
    ; symbol -> void
    (define/public #:callback (on-toggle-visibility id)
      (refresh!)
      (for/or ([control (get-controls)])
        (and (eq? (send control get-id) id)
             (send control set-visible?! (not (send control get-visible?)))
             #t)))))
