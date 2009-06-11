#lang scheme/base

(require "../../lib-base.ss"
         "html-component.ss")

(define labelled-component<%>
  (interface ()
    ; seed -> xml
    render-label
    ; -> (U xml (seed -> xml) #f)
    get-label
    ; (U xml (seed -> xml) #f) -> void
    set-label!))

(define labelled-component-mixin
  (mixin/cells (html-component<%>) (labelled-component<%>)
    
    ; (cell (U xml (seed -> xml) #f)
    (init-cell label #f #:accessor #:mutator)
    
    ; Methods ------------------------------------
    
    ; seed -> xml
    (define/public (render-label seed)
      (let ([label (get-label)])
        (cond [(procedure? label) (label seed)]
              [label              label]
              [else               (xml)])))))

; Provide statements -----------------------------

(provide labelled-component<%>
         labelled-component-mixin)


