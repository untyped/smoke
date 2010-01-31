#lang scheme

(require (planet untyped/unlib:3/string)
         "../content-base.ss")

(define-page focus-page html-page% ()
  
  ; Fields ---------------------------------------
  
  ; text-field&
  (field field1 (new text-field% [id 'field1] [on-change (callback on-field-change)]) #:child)
  (field field2 (new text-field% [id 'field2] [on-change (callback on-field-change)]) #:child)
  
  ; Constructor ----------------------------------
  
  (super-new [title "Focus"])
  
  ; Methods --------------------------------------
  
  ; -> void
  (define/public #:callback (on-field-change)
    ; Changing the values of the cells replaces the cells on the target page:
    (send field1 set-value! (format "~a." (or (send field1 get-value) "")))
    (send field2 set-value! (format "~a." (or (send field2 get-value) ""))))
  
  ; seed -> xml
  (define/augment (render seed)
    (xml (table (tr (td ,(send field1 render seed))
                    (td "Field 1"))
                (tr (td ,(send field2 render seed))
                    (td "Field 2")))
         (br)
         (p "This table is just to reveal any slowness from DOM traversal:")
         (table ,@(for/list ([y (in-range 0 100)])
                    (xml (tr ,@(for/list ([x (in-range 0 100)])
                                 (xml (td ,x "," ,y))))))))))
