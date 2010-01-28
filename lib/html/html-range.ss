#lang scheme/base

(require srfi/26/cut
         "../../lib-base.ss"
         "html-element.ss")

; Components -------------------------------------

(define-class html-range% html-element% ()
  
  (inherit core-html-attributes)
  
  ; Fields -------------------------------------
  
  ; (cell (U xml (seed -> xml)))
  (init-cell content #:accessor #:mutator)
  
  ; Constructor --------------------------------
  
  (super-new)
  
  ; Methods ------------------------------------
  
  ; seed -> xml
  (define/override (render seed)
    (xml (span (@ ,(core-html-attributes seed))
               ,(let ([content (get-content)])
                  (if (procedure? content)
                      (content seed)
                      content))))))

; Provide statements -----------------------------

(provide html-range%)

