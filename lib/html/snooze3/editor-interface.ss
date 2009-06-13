#lang scheme/base

(require (planet untyped/snooze:3)
         "../../../lib-base.ss"
         "../html-component.ss"
         "../html-element.ss"
         "../labelled-element.ss"
         "checkable.ss"
         "util.ss")

; Interfaces -------------------------------------

(define editor<%>
  (interface (checkable<%>)
    get-editors          ; -> (listof editor<%>)
    parse                ; -> (listof check-result)
    validate             ; -> (listof check-result)
    editor-changed?))    ; -> boolean

; Mixins -----------------------------------------

(define simple-editor-mixin
  (mixin/cells (html-component<%>) (editor<%>)
    
    ; Fields -------------------------------------
    
    (super-new)
    
    ; (listof editor<%>)
    (init-field editors null #:accessor #:children)
        
    ; Methods ------------------------------------
    
    ; (listof check-result) -> void
    (define/public (set-check-results! results)
      (for-each (cut send <> set-check-results! results)
                (get-editors)))
    
    ; -> (listof check-result)
    (define/public (parse)
      (apply check-problems (map (cut send <> parse) (get-editors))))
    
    ; -> (listof check-result)
    (define/public (validate)
      (apply check-problems (map (cut send <> validate)
                                   (get-editors))))
    
    ; -> boolean
    (define/public (editor-changed?)
      (ormap (cut send <> editor-changed?) (get-editors)))))

; Classes ----------------------------------------

(define simple-editor%
  (simple-editor-mixin html-element%))

; Provide statements -----------------------------

(provide editor<%>
         simple-editor-mixin
         simple-editor%)
