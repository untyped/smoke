#lang scheme/base

(require (planet untyped/snooze:3)
         "../../../../lib-base.ss"
         "../../submit-button.ss"
         "../../html-element.ss"
         "../editor.ss"
         "default-abstract.ss"
         "interfaces.ss")

; Mixins -----------------------------------------

; Sensible defaults for deleting a struct
(define (default-delete-mixin)
  (mixin/cells (html-element<%> crud-element<%> crudl-element<%> crudl-review+delete+list<%> crudl-editor<%> editor-controller<%> entity-editor<%>) 
    (crudl-review+delete<%> crudl-editor<%>)
    
    (inherit get-value
             render-struct)
    
    ; Fields -------------------------------------
    
    ; submit-button%
    (field delete-button 
           (new submit-button% [action (callback on-update)] [label (get-button-label)]) 
           #:accessor #:child)
    
    ; Methods ------------------------------------
    ; seed -> xml
    (define/augment (render seed)
      (xml ,(render-struct seed (get-value))
           ,(send delete-button render seed)))
    
    ; -> string
    (define/override (get-button-label)
      "Delete")
    
    ; Validation ---------------------------------
    ; -> (listof check-result)
    (define/override (validate)
      (error "validate must be overridden")) ; apply the validation procedure
    
    ; -> struct
    (define/override (commit-changes)
      (call-with-transaction
       (lambda () (delete! (get-value))))
      (clear-continuation-table!))))


; Provides ---------------------------------------
(provide (all-defined-out))