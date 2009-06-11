#lang scheme/base

(require (planet untyped/snooze:3)
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
         "../labelled-component.ss"
         "../password-field.ss"
         "../radio-button.ss"
         "../regexp-field.ss"
         "../set-selector.ss"
         "../text-area.ss"
         "../text-field.ss"
         "../tiny-mce.ss"
         "check-label.ss"
         "util.ss")

; Interfaces -------------------------------------

(define editor<%>
  (interface (form-element<%>)
    get-child-editors ; -> (listof editor<%>)
    parse             ; -> (listof check-result)
    validate))        ; -> (listof check-result)

(define entity-editor<%>
  (interface (editor<%>)
    ))

(define attribute-editor<%>
  (interface (labelled<%> editor<%> check-label<%>)
    get-attributes ; -> (listof attribute)
    restructure    ; snooze-struct -> snooze-struct
    destructure!)) ; snooze-struct -> void

; Mixins -----------------------------------------

(define attribute-editor-mixin
  (mixin/cells (form-element<%> labelled<%> check-label<%>) (attribute-editor<%>)
    
    (inherit get-id 
             get-value
             set-value!
             render-label
             render-check-label
             set-label!)
    
    ; Fields -------------------------------------
    
    ; (listof attribute)
    (init-cell attributes null #:accessor)
    
    ; Constructor --------------------------------
    
    (super-new)
    
    (set-label! (xml-quote (string-titlecase (if (pair? attributes)
                                                 (attribute-pretty-name (car attributes))
                                                 (symbol->string (get-id))))))
    
    ; Methods ------------------------------------
    
    ; -> (listof editor<%>)
    (define/public (get-child-editors)
      null)
    
    ; check-result -> boolean
    (define/override (report-result? result)
      (or (memq this (check-result-annotation result ann:form-elements))
          (ormap (cut check-result-has-attribute? result <>)
                 (get-attributes))))
    
    ; -> symbol
    (define/public (get-wrapper-id)
      (symbol-append (get-id) '-wrapper))
    
    ; seed -> xml
    (define/override (render seed)
      (define id (get-wrapper-id))
      (xml (span (@ [id ,id])
                 ,(super render seed) " "
                 ,(render-check-label seed))))
    
    ; snooze-struct -> snooze-struct
    (define/public (destructure! struct)
      (match (get-attributes)
        [(list (? attribute? attr))
         (set-value! (snooze-struct-ref struct attr))]
        [attrs (error "editing multiple attributes: destructure! must be overridden" attrs)]))
    
    ; snooze-struct -> snooze-struct
    (define/public (restructure struct)
      (match (get-attributes)
        [(list (? attribute? attr))
         (snooze-struct-set struct attr (get-value))]
        [attrs (error "editing multiple attributes: restructure must be overridden" attrs)]))
    
    ; -> (listof check-result)
    (define/public (parse)
      (with-handlers
          ([exn:smoke:form?
            (lambda (exn)
              (check/annotate ([ann:form-elements (list this)]
                               [ann:attrs         (get-attributes)])
                (check-fail (exn-message exn))))])
        (get-value)
        null))
    
    ; -> (listof check-result)
    (define/public (validate)
      null)
    
    ; seed -> js
    (define/override (get-on-render seed)
      (js (!dot Smoke (insertHTML (!dot Smoke (findById ,(get-wrapper-id)))
                                  "replace"
                                  ,(xml->string (render seed))))))))

(define entity-editor-mixin
  (mixin/cells (html-element<%>) (entity-editor<%>)
    
    ; (listof editor<%>)
    (field child-editors null #:accessor #:children)
    
    ; (U snooze-struct #f)
    (cell initial-value #f #:accessor)
    
    ; Constructor --------------------------------
    
    ; (listof editor<%>)
    (init [(init-child-editors editors)])
    
    (set! child-editors init-child-editors)
    
    ; Methods ------------------------------------
    
    ; -> snooze-struct
    (define/public (get-value)
      (let ([init (get-initial-value)])
        (if (snooze-struct? init)
            (for/fold ([struct (get-initial-value)])
                      ([editor (in-list child-editors)])
                      (send editor restructure struct))
            (raise-type-error 'entity-editor.get-value "snooze-struct" #f))))
    
    ; snooze-struct -> void
    (define/public (set-value! struct)
      (web-cell-set! initial-value-cell struct)
      (for ([editor (in-list child-editors)])
        (send editor destructure! struct)))
    
    ; -> boolean
    (define/public (value-valid?)
      (not (check-failures? (check-snooze-struct (get-value)))))
    
    ; -> boolean
    (define/public (value-changed?)
      (for/or ([editor (in-list (get-child-editors))])
        (send editor value-changed?)))
    
    ; -> (listof check-result)
    (define/public (parse)
      (apply check-problems
             (for/list ([editor (in-list (get-child-editors))])
               (check/annotate ([ann:form-elements (list editor)])
                 (let ([message (with-handlers ([exn:smoke:form? exn-message])
                                  (send editor get-value)
                                  #f)])
                   (if message
                       (check-fail message)
                       (check-pass)))))))
    
    ; -> (listof check-result)
    (define/public (validate)
      (check-snooze-struct (get-value)))))

; Classes ----------------------------------------

(define complete-attribute-editor-mixin
  (compose attribute-editor-mixin check-label-mixin label-mixin))

(define autocomplete-editor%              (complete-attribute-editor-mixin autocomplete-field%))
(define check-box-editor%                 (attribute-editor-mixin (check-label-mixin check-box%)))
(define combo-box-editor%                 (complete-attribute-editor-mixin combo-box%))
(define vanilla-combo-box-editor%         (complete-attribute-editor-mixin vanilla-combo-box%))
(define date-editor%                      (complete-attribute-editor-mixin date-field%))
(define file-editor%                      (complete-attribute-editor-mixin file-field%))
(define integer-editor%                   (complete-attribute-editor-mixin integer-field%))
(define password-editor%                  (complete-attribute-editor-mixin password-field%))
(define regexp-editor%                    (complete-attribute-editor-mixin regexp-field%))
(define set-selector-editor%              (complete-attribute-editor-mixin set-selector%))
(define set-selector-autocomplete-editor% (complete-attribute-editor-mixin set-selector-autocomplete%))
(define string-editor%                    (complete-attribute-editor-mixin text-field%))
(define text-editor%                      (complete-attribute-editor-mixin text-area%))
(define tiny-mce-editor%                  (complete-attribute-editor-mixin tiny-mce%))

; Provide statements -----------------------------

(provide editor<%>
         attribute-editor-mixin
         entity-editor-mixin
         autocomplete-editor%
         check-box-editor%
         combo-box-editor%
         vanilla-combo-box-editor%
         date-editor%
         file-editor%
         integer-editor%
         password-editor%
         regexp-editor%
         set-selector%
         set-selector-autocomplete%
         string-editor%
         text-editor%
         tiny-mce-editor%)
