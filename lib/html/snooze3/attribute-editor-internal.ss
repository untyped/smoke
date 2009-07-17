#lang scheme/base

(require (planet untyped/snooze:3)
         (planet untyped/unlib:3/enumeration)
         (planet untyped/unlib:3/symbol)
         "../../../lib-base.ss"
         "../autocomplete-field.ss"
         "../check-box.ss"
         "../combo-box.ss"
         "../date-field.ss"
         "../file-field.ss"
         "../form-element.ss"
         "../html-element.ss"
         "../integer-field.ss"
         "../labelled-element.ss"
         "../number-field.ss"
         "../password-field.ss"
         "../radio-button.ss"
         "../regexp-field.ss"
         "../set-selector.ss"
         "../text-area.ss"
         "../text-field.ss"
         "../tiny-mce.ss"
         "../time-field.ss"
         "check-label.ss"
         "editor-internal.ss")

; Interfaces -------------------------------------

(define attribute-editor<%>
  (interface (editor<%> labelled-element<%> check-label<%>)
    get-attributes          ; -> (listof attribute)
    destructure!
    restructure))

; Mixins -----------------------------------------

(define attribute-editor-mixin
  (mixin/cells (form-element<%> labelled-element<%> editor<%> check-label<%>) (attribute-editor<%>)
    
    (inherit get-component-id
             get-id 
             set-id!
             get-value
             set-value!
             render-label
             render-check-label
             set-label!
             value-changed?)
    
    ; Fields -------------------------------------
    
    (super-new)
    
    ; (listof attribute)
    (init-cell attributes null #:accessor)
    
    ; boolean
    (init-field required?
      (and (pair? attributes)
           (let ([attr (car attributes)])
             (not (type-allows-null? (attribute-type attr)))))
      #:accessor)
    
    (init [id    (or (attributes->id attributes) (get-component-id))]
          [label (or (attributes->label attributes) (xml-quote id))])
    
    (set-id! id)
    (set-label! label)
    
    ; Methods ------------------------------------
    
    ; check-result -> boolean
    (define/override (report-result? result)
      (or (super report-result? result)
          (ormap (cut check-result-has-attribute? result <>)
                 (get-attributes))))
    
    ; -> symbol
    (define/public (get-wrapper-id)
      (symbol-append (get-id) '-wrapper))
    
    ; seed -> xml
    (define/override (render seed)
      (xml (span (@ [id ,(get-wrapper-id)])
                 ,(super render seed) " "
                 ,(opt-xml required? "(required) ")
                 ,(render-check-label seed))))
    
    ; snooze-struct -> snooze-struct
    (define/public (destructure! struct)
      (match (get-attributes)
        [(list-rest (? attribute? attr) _)
         (set-value! (snooze-struct-ref struct attr))]
        [attrs (raise-type-error 'attribute-editor.destructure! "(list attribute attribute ...)" attrs)]))
    
    ; snooze-struct -> snooze-struct
    (define/public (restructure struct)
      (match (get-attributes)
        [(list-rest (? attribute? attr) _)
         (snooze-struct-set struct attr (get-value))]
        [attrs (raise-type-error 'attribute-editor.restructure "(list attribute attribute ...)" attrs)]))
    
    ; -> (listof check-result)
    (define/override (validate)
      (check/annotate ([ann:form-elements (list this)]
                       [ann:attrs         (get-attributes)])
        (check-until-problems
         (cut super validate)
         (cut with-handlers ([exn:smoke:form? (lambda (exn) (check-fail (exn-message exn)))])
              (if (and required? (not (get-value)))
                  (check-fail "Value is required.")
                  (check-pass))))))
        
    ; seed -> js
    (define/override (get-on-render seed)
      (js (!dot Smoke (insertHTML (!dot Smoke (findById ,(get-wrapper-id)))
                                  "replace"
                                  ,(xml->string (render seed))))))))

(define complete-attribute-editor-mixin
  (compose attribute-editor-mixin
           check-label-mixin
           simple-editor-mixin
           labelled-element-mixin))

; Classes ----------------------------------------

(define simple-attribute-editor%
  (class/cells (labelled-element-mixin (check-label-mixin (simple-editor-mixin form-element%))) (attribute-editor<%>)
    
    (inherit core-html-attributes
             get-component-id
             set-id!
             render-check-label
             set-label!)
    
    ; Fields -------------------------------------
    
    ; (listof attribute)
    (init-cell attributes null #:accessor)
    
    ; boolean
    (init-field required?
      (and (pair? attributes)
           (let ([attr (car attributes)])
             (not (type-allows-null? (attribute-type attr)))))
      #:accessor)
    
    (init [id    (or (attributes->id attributes) (get-component-id))]
          [label (if (pair? attributes)
                     (let ([attr (car attributes)])
                       (xml-quote (string-titlecase (attribute-pretty-name attr))))
                     (xml-quote id))])
    
    (super-new [id id] [label label])
    
    ; Methods ------------------------------------
    
    ; check-result -> boolean
    (define/override (report-result? result)
      (or (super report-result? result)
          (ormap (cut check-result-has-attribute? result <>)
                 (get-attributes))))
    
    ; seed -> xml
    (define/overment (render seed)
      (xml (span (@ ,(core-html-attributes seed))
                 ,(inner (xml) render seed) " "
                 ,(opt-xml required? "(required) ")
                 ,(render-check-label seed))))

    ; snooze-struct -> snooze-struct
    (define/public (destructure! struct)
      (error "simple-attribute-editor.destructure! must be overridden"))
    
    ; snooze-struct -> snooze-struct
    (define/public (restructure struct)
      (error "simple-attribute-editor.restructure must be overridden"))))

; Helpers ----------------------------------------

; (listof attribute) -> (U symbol #f)
(define (attributes->id attributes)
  (and (pair? attributes)
       (let ([attr (car attributes)])
         (gensym/interned (html-id-encode (format "~a-~a" (entity-name (attribute-entity attr)) (attribute-name attr)))))))

; (listof attribute) -> (U xml #f)
(define (attributes->label attributes)
  (and (pair? attributes)
       (let ([attr (car attributes)])
         (xml-quote (string-titlecase (attribute-pretty-name attr))))))

; Provide statements -----------------------------

(provide attribute-editor<%>
         attribute-editor-mixin
         complete-attribute-editor-mixin
         simple-attribute-editor%)
