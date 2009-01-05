#lang scheme/base

(require "../../lib-base.ss"
         "html-element.ss")

; Interfaces -------------------------------------

(define form-element<%>
  (interface (html-element<%>)
    get-enabled?     ; -> boolean
    set-enabled?!    ; boolean -> void
    get-value        ; -> any
    set-value!       ; any -> void
    value-valid?     ; -> boolean
    get-value-error  ; -> (U string #f)
    value-changed?)) ; -> boolean

; Mixins -----------------------------------------

(define form-element-mixin
  (mixin/cells (html-element<%>) (form-element<%>)
    
    (inherit get-id get-classes get-style get-tooltip)
    
    ; Fields -------------------------------------
    
    ; (cell boolean)
    (init-cell [enabled? #t] #:accessor #:mutator)
    
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
      (define enabled? (get-enabled?))
      (unless id (error (format "ID not set in form-element: ~a" this)))
      (append (super core-html-attributes seed
                     #:id      id
                     #:classes classes
                     #:style   style
                     #:tooltip title)
              (if enabled? 
                  (xml-attrs [name ,id])
                  (xml-attrs [name ,id] [disabled "disabled"]))))))

(define form-element%
  (class/cells (form-element-mixin html-element%) ()))

; Provide statements -----------------------------

(provide form-element<%>
         form-element-mixin
         form-element%)
