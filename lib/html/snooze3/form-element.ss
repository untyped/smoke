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
         "../number-field.ss"
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
    get-editors ; -> (listof editor<%>)
    parse       ; -> (listof check-result)
    validate))  ; -> (listof check-result)

(define entity-editor<%>
  (interface (editor<%>)
    get-entity))

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
    
    ; boolean
    (init-field required? #f #:accessor)
    
    ; Constructor --------------------------------
    
    (super-new)
    
    (set-label! (xml-quote (string-titlecase (if (pair? attributes)
                                                 (attribute-pretty-name (car attributes))
                                                 (symbol->string (get-id))))))
    
    ; Methods ------------------------------------
    
    ; -> (listof editor<%>)
    (define/public (get-editors)
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
                 ,(opt-xml required? "(required) ")
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
    
    (inherit core-html-attributes)
    
    ; Fields -------------------------------------
    
    ; entity
    (init-field entity #:accessor)
    
    ; (listof editor<%>)
    (init-field editors (default-entity-editors entity) #:accessor #:children)
    
    ; (U snooze-struct #f)
    (cell initial-value #f #:accessor)
    
    ; Methods ------------------------------------
    
    ; seed -> xml
    (define/override (render seed)
      (xml (table (@ ,(core-html-attributes seed))
                  ,@(for/list ([editor (in-list (get-editors))])
                      (xml (tr (th ,(send editor render-label seed))
                               (td ,(send editor render seed))))))))
    
    ; -> snooze-struct
    (define/public (get-value)
      (let ([init (get-initial-value)])
        (if (snooze-struct? init)
            (for/fold ([struct (get-initial-value)])
                      ([editor (in-list editors)])
                      (send editor restructure struct))
            (raise-type-error 'entity-editor.get-value "snooze-struct" #f))))
    
    ; snooze-struct -> void
    (define/public (set-value! struct)
      (unless (snooze-struct? struct)
        (raise-type-error 'entity-editor.set-value! "snooze-struct" struct))
      (web-cell-set! initial-value-cell struct)
      (for ([editor (in-list editors)])
        (send editor destructure! struct)))
    
    ; -> boolean
    (define/public (value-valid?)
      (not (check-failures? (check-snooze-struct (get-value)))))
    
    ; -> boolean
    (define/public (value-changed?)
      (for/or ([editor (in-list (get-editors))])
        (send editor value-changed?)))
    
    ; -> (listof check-result)
    (define/public (parse)
      (apply check-problems
             (for/list ([editor (in-list (get-editors))])
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

; Helpers ----------------------------------------

(define (default-entity-editors entity)
  (for/list ([attr (in-list (cddr (entity-attributes entity)))])
    (let* ([id        (symbol-append (entity-name entity) '- (attribute-name attr) '-field)]
           [label     (string-titlecase (attribute-pretty-name attr))]
           [type      (attribute-type attr)]
           [required? (not (type-allows-null? type))])
      (match type
        [(? boolean-type?)                   (new check-box-editor% [id id] [label label] [attributes (list attr)] [required? required?])]
        [(? integer-type?)                   (new integer-editor%   [id id] [label label] [attributes (list attr)] [required? required?])]
        [(? real-type?)                      (new number-editor%    [id id] [label label] [attributes (list attr)] [required? required?])]
        [(struct string-type (_ max-length)) (if max-length
                                                 (new text-field-editor% [id id] [label label] [attributes (list attr)] [required? required?] [max-length max-length])
                                                 (new text-area-editor%  [id id] [label label] [attributes (list attr)] [required? required?]))]
        [(struct symbol-type (_ max-length)) (if max-length
                                                 (new (symbol-editor-mixin text-field-editor%)
                                                      [id         id]
                                                      [label      label]
                                                      [attributes (list attr)]
                                                      [required?  required?]
                                                      [max-length max-length])
                                                 (new (symbol-editor-mixin text-area-editor%)
                                                      [id         id]
                                                      [label      label]
                                                      [attributes (list attr)]
                                                      [required?  required?]))]
        [(struct guid-type (_ entity))       (new foreign-key-editor% [id id] [label label] [attributes (list attr)] [required? required?] [entity entity])]))))

(define symbol-editor-mixin
  (mixin/cells (editor<%>) ()

    ; -> symbol
    (define/override (get-value)
      (let ([str (super get-value)])
        (and str (string->symbol str))))
    
    ; symbol -> void
    (define/override (set-value! sym)
      (super set-value! (and sym (symbol->string sym))))))

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
(define number-editor%                    (complete-attribute-editor-mixin number-field%))
(define password-editor%                  (complete-attribute-editor-mixin password-field%))
(define regexp-editor%                    (complete-attribute-editor-mixin regexp-field%))
(define set-selector-editor%              (complete-attribute-editor-mixin set-selector%))
(define set-selector-autocomplete-editor% (complete-attribute-editor-mixin set-selector-autocomplete%))
(define text-field-editor%                (complete-attribute-editor-mixin text-field%))
(define text-area-editor%                 (complete-attribute-editor-mixin text-area%))
(define tiny-mce-editor%                  (complete-attribute-editor-mixin tiny-mce%))

(define foreign-key-editor%
  (class/cells combo-box-editor% ()
    
    ; Fields -------------------------------------
    
    ; (U entity #f)
    (init-field entity #f #:accessor)
    
    ; Methods ------------------------------------
    
    ; -> (listof (cons integer string))
    (define/override (get-options)
      (let-sql ([entity entity])
        (list* #f (select-all #:from entity #:order ((asc entity.guid))))))
    
    ; (U snooze-struct #f) -> integer
    (define/override (option->raw option)
      (and option (guid? option) (snooze-struct-id option)))
    
    ; (U integer #f) -> snooze-struct
    (define/override (raw->option raw)
      (and raw 
           (let ([id (string->number raw)])
             (and id (find-by-id entity id)))))
    
    ; (U snooze-struct #f) -> string
    (define/override (option->string option)
      (if option
          (format-snooze-struct option)
          (format "-- No ~a selected --" (entity-pretty-name entity))))))

; Provide statements -----------------------------

(provide editor<%>
         attribute-editor<%>
         attribute-editor-mixin
         entity-editor<%>
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
         text-field-editor%
         text-area-editor%
         tiny-mce-editor%)
