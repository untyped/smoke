#lang scheme/base

(require "../../lib-base.ss"
         "html-element.ss"
         "form-element.ss")

(define labelled-element<%>
  (interface (html-element<%>)
    ; seed -> xml
    render-label
    ; seed -> xml
    render-label-content
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
    (define/public (render-label-content seed)
      (let ([label (get-label)])
        (if (procedure? label)
            (label seed)
            label)))
    
    ; seed -> xml
    (define/public (render-label seed)
      (let ([id (get-id)])
        (opt-xml (get-label)
          ,(if (is-a? this form-element<%>)
               (xml (label (@ [for ,id])
                           ,(render-label-content seed)))
               (render-label-content seed)))))))

; Provide statements -----------------------------

(provide labelled-element<%>
         labelled-element-mixin)


