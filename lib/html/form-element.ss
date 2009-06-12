#lang scheme/base

(require "../../lib-base.ss"
         "disableable-element.ss"
         "html-element.ss")

; Interfaces -------------------------------------

(define form-element<%>
  (interface (disableable-element<%>)
    get-value        ; -> any | exn:smoke:form
    set-value!       ; any -> void
    value-valid?     ; -> boolean
    value-changed?)) ; -> boolean

; Mixins -----------------------------------------

(define form-element-mixin    
  (mixin/cells (disableable-element<%>) (form-element<%>)
    
    (inherit get-classes
             get-component-id
             get-enabled?
             get-id
             get-style
             get-tooltip)    
    
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
              (xml-attrs [name ,id])))
    
    ; Printing -----------------------------------
    
    ; output-port boolean (U symbol #f) -> void
    (define/override (custom-print out write? class-name)
      (fprintf out
               (if write? "#(~a ~a ~a ~s)" "#(~a ~a ~a ~a)")
               (or class-name 'unknown-form-element)
               (get-component-id)
               (get-id)
               (with-handlers ([exn:smoke:form? (lambda _ (format "bad value: ~a" (get-value-error)))])
                 (get-value))))))

(define form-element%
  (class/cells (form-element-mixin disableable-element%) ()))

; Provide statements -----------------------------

(provide form-element<%>
         form-element-mixin
         form-element%)
