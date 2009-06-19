#lang scheme

(require "../../lib-base.ss"
         "html-element.ss")

(define html-box%
  (class/cells html-element% ()
    
    (inherit core-html-attributes)

    ; Fields -------------------------------------
    
    ; (cell (U html-component% #f))
    (cell content #f #:optional-child #:accessor #:mutator)
    
    ; Methods ------------------------------------
    
    ; seed -> xml
    (define/overment (render seed)
      (xml (div (@ ,@(core-html-attributes seed))
                ,(opt-xml (get-content)
                   ,(send (get-content) render seed)))))))

; Provide statements -----------------------------

(provide html-box%)
