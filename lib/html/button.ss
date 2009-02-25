#lang scheme/base

(require (planet untyped/unlib:3/symbol)
         "../../lib-base.ss"
         "disableable-element.ss"
         "html-component.ss")

; Classes ----------------------------------------

(define button%
  (class/cells disableable-element% ()
    
    (inherit core-html-attributes)
    
    ; Fields -------------------------------------
    
    ; (cell string)
    (init-cell [label #f] #:accessor #:mutator)
    
    ; Constructor --------------------------------
    
    (init [classes null])
    
    (super-new [classes (cons 'smoke-button classes)])
    
    ; Public methods ---------------------------
    
    ; seed -> xml
    (define/override (render seed)
      (xml (input (@ ,@(core-html-attributes seed)
                     [type "button"]
                     [value ,(get-label)]))))))

; Provide statements -----------------------------

(provide button%)