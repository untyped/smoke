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
    
    ; Constructor --------------------------------
    
    ; (listof (U symbol string))
    (init [classes null])
    
    (super-new [classes (cons 'ui-widget classes)])
    
    ; Public methods -----------------------------
    
    ; -> any
    (define/public (get-value)
      (error "form-element.get-value must be overridden."))
    
    ; any -> void
    (define/public (set-value! val)
      (error "form-element.set-value! must be overridden."))
    
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
    ; Does NOT output the value of the form element.
    (define/override (core-html-attributes 
                      seed
                      #:id      [id      (get-id)]
                      #:classes [classes (get-classes)]
                      #:name    [name    id]
                      #:style   [style   (get-style)]
                      #:tooltip [title   (get-tooltip)])
      (unless id (error (format "ID not set in form-element: ~a" this)))
      (append (super core-html-attributes seed
                     #:id      id
                     #:classes classes
                     #:style   style
                     #:tooltip title)              
              (opt-xml-attr name)))
    
    ; Printing -----------------------------------
    
    ; output-port (any output-port -> void) (U symbol #f) -> void
    (define/override (custom-print out print class-name)
      (print (vector (or class-name 'unknown-form-element)
                     (with-handlers ([exn? (lambda (exn) '<no-component-id>)])
                       (get-component-id))
                     (with-handlers ([exn? (lambda (exn) '<no-id>)])
                       (get-id))
                     (with-handlers ([exn? (lambda (exn) '<no-value>)])
                       (let ([err (get-value-error)])
                         (if err
                             (list 'bad-value err)
                             (get-value)))))
             out))))

(define form-element%
  (class/cells (form-element-mixin disableable-element%) ()))

; Provide statements -----------------------------

(provide form-element<%>
         form-element-mixin
         form-element%)
