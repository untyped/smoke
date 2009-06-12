#lang scheme/base

(require "../../../lib-base.ss"
         "../html-component.ss"
         "../html-element.ss"
         "checkable.ss")

; Interfaces -------------------------------------

(define view<%>
  (interface (checkable<%>)
    get-views)) ; -> (listof view<%>)

; Mixins -----------------------------------------

(define simple-view-mixin
  (mixin/cells (html-component<%>) (view<%>)
    
    ; Fields -------------------------------------
    
    (super-new)
    
    ; (listof view<%>)
    (init-field views null #:accessor #:children)
        
    ; Methods ------------------------------------
    
    ; (listof check-result) -> void
    (define/public (set-check-results! results)
      (for-each (cut send <> set-check-results! results) (get-views)))))

; Classes ----------------------------------------

(define simple-view%
  (simple-view-mixin html-element%))

; Provide statements -----------------------------

(provide checkable<%>
         view<%>
         simple-view-mixin
         simple-view%)
