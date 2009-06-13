#lang scheme/base

(require "../../lib-base.ss"
         "html-element.ss"
         "form-element.ss")

(define labelled-element<%>
  (interface (html-element<%>)
    ; seed -> xml
    render-label
    ; -> (U xml (seed -> xml) #f)
    get-label
    ; (U xml (seed -> xml) #f) -> void
    set-label!))

(define labelled-element-mixin
  (mixin/cells (html-element<%>) (labelled-element<%>)
    
    (inherit get-id)
    
    ; Fields -------------------------------------
    
    ; (cell (U xml (seed -> xml) #f)
    (init-cell label #f #:accessor #:mutator)
    
    ; Methods ------------------------------------
    
    ; seed -> xml
    (define/public (render-label seed)
      (let ([id    (get-id)]
            [label (get-label)])
        (opt-xml label
          ,(if (is-a? this form-element<%>)
               (xml (label (@ [for ,id])
                           ,(if (procedure? label)
                                (label seed)
                                label)))
               (if (procedure? label)
                   (label seed)
                   label)))))))

; Provide statements -----------------------------

(provide labelled-element<%>
         labelled-element-mixin)


