#lang scheme/base

(require srfi/26/cut
         "../../lib-base.ss"
         "html-component.ss")

; Components -------------------------------------

(define html-range%
  (class/cells html-component% ()
    
    ; Fields -------------------------------------
    
    ; (cell (seed -> xml))
    (init-cell content
      #:accessor)
    
    ; Constructor --------------------------------
    
    (super-new)
    
    ; Methods ------------------------------------

    ; (U xml (seed -> xml)) -> void
    (define/public (set-content! content)
      (web-cell-set! 
       content-cell
       (if (procedure? content)
           content
           (lambda (seed) content))))
    
    ; seed -> xml
    (define/override (render seed)
      ((get-content) seed))))

; Constructors -----------------------------------

; xml -> html-range%
(define (static-range content)
  (new html-range% [content (lambda (seed) content)]))

; (seed -> xml) -> html-range%
(define (dynamic-range content)
  (new html-range% [content content]))

; Provide statements -----------------------------

(provide html-range%)

(provide/contract
 [static-range  (-> xml? (is-a?/c html-range%))]
 [dynamic-range (-> (-> seed? xml?) (is-a?/c html-range%))])
