#lang web-server

(require srfi/13/string
         "../../lib-base.ss"
         "html-component.ss")

; Mixins -----------------------------------------

(define refreshable-mixin
  (mixin/cells (html-component<%>) ()
    
    ; Fields -------------------------------------
    
    ; (cell boolean)
    (init-cell refresh? #f #:accessor)
    
    ; Methods ------------------------------------
    
    ; -> void
    (define/public (refresh!)
      (web-cell-set! refresh?-cell #t))
    
    ; -> boolean
    (define/override (dirty?)
      (or (for/fold ([ans #f])
                    ([frame (in-frames)])
                    (or (begin0
                          (and (web-cell-set? refresh?-cell frame)
                               (web-cell-ref refresh?-cell frame))
                          (web-cell-unset! refresh?-cell frame))
                        ans))
          (super dirty?)))))

; Provide statements -----------------------------

(provide refreshable-mixin)
