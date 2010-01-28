#lang scheme/base

(require srfi/13/string
         "../../lib-base.ss"
         "html-component.ss")

; Mixins -----------------------------------------

(define refreshable-mixin
  (mixin/cells (html-component<%>) ()
    
    ; Fields -------------------------------------
    
    ; (cell (thread-cell boolean))
    (field refresh-cell (make-thread-cell #f))
    
    ; Constructor --------------------------------
    
    (init [refresh? #f])
    
    (when refresh? (refresh!))
    
    ; Methods ------------------------------------
    
    ; -> void
    (define/public (refresh!)
      (thread-cell-set! refresh-cell #t))
    
    ; -> boolean
    (define/override (dirty?)
      (or (thread-cell-ref refresh-cell)
          (super dirty?)))))

; Provide statements -----------------------------

(provide refreshable-mixin)
