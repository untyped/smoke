#lang scheme/base

(require (planet untyped/unlib:3/symbol)
         "../../lib-base.ss"
         "html-component.ss"
         "html-element.ss")

; Classes ----------------------------------------

(define submit-button%
  (class/cells html-element% ()
    
    (inherit core-html-attributes
             get-id)
    
    ; Fields -------------------------------------
    
    ; (cell string)
    (init-cell action #:accessor #:mutator)
    
    ; (cell string)
    (init-cell [label "OK"] #:accessor #:mutator)
    
    ; Constructor --------------------------------
    
    (init [classes null])
    
    (super-new [classes (cons 'smoke-submit-button classes)])
    
    ; Public methods ---------------------------
    
    ; seed -> xml
    (define/override (render seed)
      (xml (input (@ ,@(core-html-attributes seed)
                     [type "submit"]
                     [value ,(get-label)]))))
    
    ; seed -> js
    (define/augment (get-on-click seed)
      (js (= (!dot this form action) ,(embed seed (get-action)))
          ,(inner (js) get-on-click seed)))))

; Provide statements -----------------------------

(provide submit-button%)
