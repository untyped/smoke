#lang scheme/base

(require "../../lib-base.ss"
         "disableable-element.ss"
         "html-element.ss")

; Interfaces -------------------------------------

(define form-element<%>
  (interface (disableable-element<%>)
    get-value        ; -> any
    set-value!       ; any -> void
    value-valid?     ; -> boolean
    get-value-error  ; -> (U string #f)
    value-changed?)) ; -> boolean

; Mixins -----------------------------------------

(define form-element-mixin    
  (mixin/cells (disableable-element<%>) (form-element<%>)
    
    (inherit get-id get-classes get-enabled? get-style get-tooltip)    
    
    ; Public methods -----------------------------
    
    ; -> any
    (define/public (get-value)
      (error "get-value must be overridden."))
    
    ; any -> void
    (define/public (set-value! val)
      (error "set-value! must be overridden."))
    
    ; -> boolean
    (define/public (value-valid?)
      (with-handlers ([exn:smoke:form? (lambda _ #f)])
        (get-value)
        #t))
    
    ; -> (U string #f)
    (define/public (get-value-error)
      (with-handlers ([exn:smoke:form? exn-message])
        (get-value)
        #f))
    
    ; -> boolean
    (define/public (value-changed?)
      (error "value-changed? must be overridden."))
    
    ; seed -> (listof attribute)
    ; Does NOT output the value.
    (define/override (core-html-attributes 
                      seed
                      #:id      [id      (get-id)]
                      #:classes [classes (get-classes)]
                      #:style   [style   (get-style)]
                      #:tooltip [title   (get-tooltip)])
      (define id       (get-id))
      (unless id (error (format "ID not set in form-element: ~a" this)))
      (append (super core-html-attributes seed
                     #:id      id
                     #:classes classes
                     #:style   style
                     #:tooltip title)              
              (xml-attrs [name ,id])))))


(define form-element%
  (class/cells (form-element-mixin disableable-element%) ()))

; Provide statements -----------------------------

(provide form-element<%>
         form-element-mixin
         form-element%)
