#lang scheme/base

(require "../../lib-base.ss"
         "html-element.ss")

; Interfaces -------------------------------------

(define disableable-element<%>
  (interface (html-element<%>)
    get-enabled?    ; -> boolean
    set-enabled?!)) ; boolean -> void

; Mixins -----------------------------------------

(define-mixin disableable-element-mixin (html-element<%>) (disableable-element<%>)
    
    (inherit get-id get-classes get-style get-tooltip)
    
    ; Fields -------------------------------------
    
    ; (cell boolean)
    (init-cell enabled? #t #:accessor #:mutator)
    
    ; Public methods -----------------------------    
    
    ; seed -> (listof attribute)
    ; Does NOT output the value.
    (define/override (core-html-attributes seed
                                           #:id      [id      (get-id)]
                                           #:classes [classes (if (get-enabled?) 
                                                                  (get-classes)
                                                                  (cons 'ui-state-disabled (get-classes)))]
                                           #:style   [style   (get-style)]
                                           #:tooltip [title   (get-tooltip)])
      (append (super core-html-attributes seed
                     #:id      id
                     #:classes classes
                     #:style   style
                     #:tooltip title)
              (if (get-enabled?)
                  null
                  (xml-attrs [disabled "disabled"])))))

; Classes ----------------------------------------

(define-class disableable-element% (disableable-element-mixin html-element%) ())

; Provide statements -----------------------------

(provide disableable-element<%>
         disableable-element-mixin
         disableable-element%)
