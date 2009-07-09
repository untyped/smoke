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
         "../radio-combo.ss"
         "../regexp-field.ss"
         "../set-selector.ss"
         "../text-area.ss"
         "../text-field.ss"
         "../tiny-mce.ss"
         "../time-field.ss"
         "attribute-editor-internal.ss"
         "check-label.ss"
         "editor-internal.ss"
         "foreign-key-editor.ss")


; Helper mixins ----------------------------------

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

(define enum-editor-mixin
  (mixin/cells (attribute-editor<%>) ()
    
    (init attributes)
    (init [options (let* ([type (and (pair? attributes) (attribute-type (car attributes)))])
                     (if (enum-type? type)
                         (enum-type-options this type)
                         (raise-exn exn:fail:contract
                           (format "enum-combo-box-editor% constructor: ~a: ~s"
                                   "options must be specified for non-enum attributes"
                                   attributes))))])
    
    (super-new [attributes attributes] [options options])))

; Classes ----------------------------------------

(define autocomplete-editor%              (complete-attribute-editor-mixin autocomplete-field%))
(define check-box-editor%                 (attribute-editor-mixin (check-label-mixin (simple-editor-mixin check-box%))))
(define combo-box-editor%                 (enum-editor-mixin (complete-attribute-editor-mixin combo-box%)))
(define vanilla-combo-box-editor%         (complete-attribute-editor-mixin vanilla-combo-box%))
(define date-editor%                      (complete-attribute-editor-mixin date-field%))
(define file-editor%                      (complete-attribute-editor-mixin file-field%))
(define integer-editor%                   (complete-attribute-editor-mixin integer-field%))
(define number-editor%                    (complete-attribute-editor-mixin number-field%))
(define password-editor%                  (complete-attribute-editor-mixin password-field%))
(define radio-combo-editor%               (enum-editor-mixin (complete-attribute-editor-mixin radio-combo%)))
(define regexp-editor%                    (complete-attribute-editor-mixin regexp-field%))
(define set-selector-editor%              (complete-attribute-editor-mixin set-selector%))
(define set-selector-autocomplete-editor% (complete-attribute-editor-mixin set-selector-autocomplete%))
(define text-field-editor%                (complete-attribute-editor-mixin text-field%))
(define text-area-editor%                 (complete-attribute-editor-mixin text-area%))
(define time-editor%                      (complete-attribute-editor-mixin time-field%))
(define tiny-mce-editor%                  (complete-attribute-editor-mixin tiny-mce%))

; Procedures -------------------------------------

; (parameter (attribute -> attribute-editor<%>))
(define attribute-editor-defaults
  (make-parameter
   (lambda (attr)
     (let* ([entity (attribute-entity attr)]
            [type   (attribute-type   attr)])
       (match type
         [(struct guid-type (_ entity)) (new foreign-key-editor% [attributes (list attr)] [entity entity])]
         [(? boolean-type?)             (new check-box-editor%   [attributes (list attr)] [show-label? #f])]
         [(? integer-type?)             (new integer-editor%     [attributes (list attr)])]
         [(? enum-type?)                (if (< (length (enum-type-values type)) 5)
                                            (new radio-combo-editor% [attributes (list attr)] [vertical? #f])
                                            (new combo-box-editor%   [attributes (list attr)]))]
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

; Helper procedures ------------------------------

; editor<%> enum-type -> (alistof (U symbol #f) string)
(define (enum-type-options editor type)
  (let* ([enum          (enum-type-enum type)]
         [values        (enum-type-values type)]
         [value->string (if enum
                            (cut enum-prettify enum <>)
                            symbol->string)])
    `(,@(if (type-allows-null? type)
            (if (is-a? editor radio-combo%)
                '((#f . "None"))
                '((#f . "-- No selection --")))
            null)
      ,@(for/list ([val (in-list values)])
          (cons val (value->string val))))))

; Provide statements -----------------------------

(provide (all-from-out "attribute-editor-internal.ss"
                       "foreign-key-editor.ss")
         autocomplete-editor%
         check-box-editor%
         combo-box-editor%
         vanilla-combo-box-editor%
         date-editor%
         file-editor%
         integer-editor%
         password-editor%
         regexp-editor%
         set-selector-editor%
         set-selector-autocomplete-editor%
         text-field-editor%
         text-area-editor%
         time-editor%
         tiny-mce-editor%)

(provide/contract
 [attribute-editor-defaults (parameter/c procedure?)]
 [default-attribute-editor  (-> attribute? (is-a?/c attribute-editor<%>))])
