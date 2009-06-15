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
         "../labelled-element.ss"
         "../number-field.ss"
         "../password-field.ss"
         ;"../radio-button.ss"
         "../regexp-field.ss"
         "../set-selector.ss"
         "../text-area.ss"
         "../text-field.ss"
         "../tiny-mce.ss"
         "../time-field.ss"
         "check-label.ss"
         "editor-interface.ss")

; Interfaces -------------------------------------

(define attribute-editor<%>
  (interface (editor<%> labelled-element<%> check-label<%>)
    get-attributes ; -> (listof attribute)
    restructure    ; snooze-struct -> snooze-struct
    destructure!)) ; snooze-struct -> void

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
    (define/override (parse)
      (check-problems
       (with-handlers ([exn:smoke:form?
                        (lambda (exn)
                          (check/annotate ([ann:form-elements (list this)]
                                           [ann:attrs         (get-attributes)])
                            (check-fail (exn-message exn))))])
         (get-value)
         null)
       (super parse)))
    
    ; -> boolean
    (define/override (editor-changed?)
      (or (value-changed?)
          (super editor-changed?)))
    
    ; seed -> js
    (define/override (get-on-render seed)
      (js (!dot Smoke (insertHTML (!dot Smoke (findById ,(get-wrapper-id)))
                                  "replace"
                                  ,(xml->string (render seed))))))))

; Classes ----------------------------------------

(define complete-attribute-editor-mixin
  (compose attribute-editor-mixin
           check-label-mixin
           simple-editor-mixin
           labelled-element-mixin))

(define autocomplete-editor%              (complete-attribute-editor-mixin autocomplete-field%))
(define check-box-editor%                 (attribute-editor-mixin (check-label-mixin (simple-editor-mixin check-box%))))
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
(define time-editor%                      (complete-attribute-editor-mixin time-field%))
(define tiny-mce-editor%                  (complete-attribute-editor-mixin tiny-mce%))

(define foreign-key-editor%
  (class/cells vanilla-combo-box-editor% ()
    
    ; Fields -------------------------------------
    
    ; (U entity #f)
    (init-field entity #:accessor)
    
    ; Methods ------------------------------------
    
    ; -> (listof (cons integer string))
    (define/override (get-options)
      (let-sql ([entity (entity-default-alias (get-entity))])
               (list* #f (select-all #:from entity #:order ((asc entity.guid))))))
    
    ; (U guid #f) -> (U integer #f)
    (define/override (option->raw option)
      (and (guid? option)
           (snooze-struct-id option)))
    
    ; (U string #f) -> guid
    (define/override (raw->option raw)
      (and raw (let ([id (string->number raw)])
                 (and id (find-by-id entity id)))))
    
    ; (U snooze-struct #f) -> string
    (define/override (option->string option)
      (if option
          (format-snooze-struct option)
          (format "-- No ~a selected --" (entity-pretty-name entity))))))

(define simple-attribute-editor%
  (class/cells (labelled-element-mixin (check-label-mixin (simple-editor-mixin html-element%))) (attribute-editor<%>)
    
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

; Procedures -------------------------------------

; (parameter (attribute -> attribute-editor<%>))
(define attribute-editor-defaults
  (make-parameter
   (lambda (attr)
     (let* ([entity    (attribute-entity attr)]
            [type      (attribute-type attr)])
       (match type
         [(struct guid-type (_ entity)) (new foreign-key-editor% [attributes (list attr)] [entity entity])]
         [(? boolean-type?)             (new check-box-editor%   [attributes (list attr)] [show-label? #f])]
         [(? integer-type?)             (new integer-editor%     [attributes (list attr)])]
         [(? enum-type?)                (new combo-box-editor%   
                                             [attributes (list attr)]
                                             [options `(,@(if (type-allows-null? type) 
                                                              (list (cons #f "-- No selection --"))
                                                              null)
                                                        ,@(for/list ([enum-val (in-list (enum-type-values type))])
                                                            (cons enum-val (symbol->string enum-val))))])]
         [(? real-type?)                (new number-editor%      [attributes (list attr)])]
         [(? time-utc-type?)            (new (time-utc-editor-mixin date-editor%) [attributes (list attr)] [size 10])]
         [(? time-tai-type?)            (new (time-tai-editor-mixin date-editor%) [attributes (list attr)] [size 10])]
         [(struct string-type (_ max-length))
          (if max-length
              (new text-field-editor% [attributes (list attr)] [size 50] [max-length max-length])
              (new text-area-editor%  [attributes (list attr)] [cols 50] [rows 5]))]
         [(struct symbol-type (_ max-length))
          (if max-length
              (new (symbol-editor-mixin text-field-editor%) [attributes (list attr)] [size 50] [max-length max-length])
              (new (symbol-editor-mixin text-area-editor%)  [attributes (list attr)] [cols 50] [rows 5]))])))))

; attribute -> attribute-editor<%>
(define (default-attribute-editor attr)
  ((attribute-editor-defaults) attr))

; Helpers ----------------------------------------

(define time-utc-editor-mixin
  (mixin/cells (date-field<%> editor<%>) ()
    (inherit get-time-utc)
    ; -> (U time-utc #f)
    (define/override (get-value)
      (get-time-utc))))

(define time-tai-editor-mixin
  (mixin/cells (date-field<%> editor<%>) ()
    (inherit get-time-tai)
    ; -> (U time-tai #f)
    (define/override (get-value)
      (get-time-tai))))

(define symbol-editor-mixin
  (mixin/cells (form-element<%> editor<%>) ()
    ; -> (U symbol #f)
    (define/override (get-value)
      (let ([str (super get-value)])
        (and str (string->symbol str))))
    ; (U symbol #f) -> void
    (define/override (set-value! sym)
      (super set-value! (and sym (symbol->string sym))))))

; (listof attribute) -> (U symbol #f)
(define (attributes->id attributes)
  (and (pair? attributes)
       (let ([attr (car attributes)])
         (gensym/interned (symbol-append (entity-name (attribute-entity attr)) '- (attribute-name attr))))))

; (listof attribute) -> (U xml #f)
(define (attributes->label attributes)
  (and (pair? attributes)
       (let ([attr (car attributes)])
         (xml-quote (string-titlecase (attribute-pretty-name attr))))))

; Provide statements -----------------------------

(provide attribute-editor<%>
         attribute-editor-mixin
         autocomplete-editor%
         check-box-editor%
         combo-box-editor%
         complete-attribute-editor-mixin
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
         time-editor%
         tiny-mce-editor%
         foreign-key-editor%
         simple-attribute-editor%
         time-utc-editor-mixin
         time-tai-editor-mixin
         symbol-editor-mixin)

(provide/contract
 [attribute-editor-defaults (parameter/c procedure?)]
 [default-attribute-editor  (-> attribute? (is-a?/c attribute-editor<%>))])
