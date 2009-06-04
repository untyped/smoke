#lang scheme/base

(require srfi/19
         (planet untyped/snooze:3)
         "../../../../lib-base.ss"
         "../../html-element.ss"
         "../editor.ss"
         "interfaces.ss")

; Mixins -----------------------------------------

; Sensible defaults for all CRUDL elements
(define (default-crudl-mixin entity)
  (mixin/cells (html-element<%>) (crudl-element<%>)
    
    ; -> entity
    (define/public-final (get-entity)
      entity)
    
    ; Skip the GUID and revision attributes, but retain all others
    ; -> (listof attribute)
    (define/public (get-attributes)
      (cddr (entity-attributes entity))) ; TODO: an abstraction like (entity-attributes-external ...) in Snooze?
    
    ; Again, skip the GUID and revision values
    ; snooze-struct -> (listof any)
    (define/public (get-struct-values struct)
      (cddr (snooze-struct-ref* struct)))  ; TODO: an abstraction like (snooze-struct-ref*-external ...) in Snooze?
    
    ; Defaults to the pretty attribute name, but may be overridden
    ; attribute -> string
    (define/public (get-attribute-pretty-name attribute)
      (attribute-pretty-name attribute))
    
    ; Take the prettified attribute name, defined with the entity
    ; seed attribute -> xml
    (define/public (render-attribute-label seed attr)
      (xml ,(get-attribute-pretty-name attr)))
    
    ; Contingent on the subclass, so do not define here.
    ; seed struct -> xml
    (define/public (render-struct seed struct)
      (error "render-struct must be overridden"))))



; Provide all single-struct CRUD pages with access to that struct
(define (default-crud-mixin)
  (mixin/cells (html-element<%>) (crud-element<%>)
    ; Fields -----------------------------------
    ; (cell (U snooze-struct #f))
    (init-cell struct #f #:accessor #:mutator)
    
    ; Methods ----------------------------------
    
    ; seed attribute -> xml
    (define/public (render-attribute seed attr)
      (error "render-attribute must be overridden"))
    
    ; seed (listof attribute) -> xml
    ; seed (listof attribute) -> xml
    (define/public-final (render-attributes seed attrs)
      (xml ,@(for/list ([attr  (in-list attrs)])
               (render-attribute seed attr))))))



; Defaults for editor mixins: force snooze-editor compliance, and customise button text
(define (default-crudl-editor-mixin)
  (let ([crudl-extension-mixin 
         (mixin/cells (snooze-editor<%>) (crudl-editor<%>)
           ; string -> submit-button
           (define/public (get-button-label)
             "Okay"))])
    (lambda (element) (crudl-extension-mixin (snooze-editor-mixin element)))))



; Defaults for review-delete-list pages (the display-only pages).
(define (default-review+delete+list-mixin)
  (mixin/cells (crudl-element<%>) (crudl-review+delete+list<%>)
    
    ; crudl-operation/c snooze struct -> (U string #f)
    (define/public (struct->crud-url crudl-operation struct)
      #f)
    
    ; Governs the decision to render as plain or foreign-key values (FINAL)
    ; seed attribute any -> xml
    (define/public-final (render-value seed attr value)
      (let ([attr-type (attribute-type attr)])
        (if (and (guid-type? attr-type) value)
            (render-value/foreign-key seed attr value)
            (render-value/plain       seed attr value))))
    
    ; Plain attributes are simply turned into strings and rendered.
    ; seed attribute any -> xml
    (define/public (render-value/plain seed attr plain)
      (let ([attr-type (attribute-type attr)])
        (xml ,(cond [(not plain) ; default for empty values is the empty string, regardless of type
                     ""]
                    [(time-utc-type? attr-type)
                     (date->string (time-utc->date plain))]
                    [(time-tai-type? attr-type)
                     (date->string (time-tai->date plain))]
                    [(boolean-type? attr-type)
                     (if plain "yes" "no")]
                    [else (format "~a" plain)]))))
    
    ; Foreign-keys are resolved to a URL if possible, and linked; otherwise they are just displayed.
    ; seed attribute snooze-struct -> xml
    (define/public-final (render-value/foreign-key seed attr struct)
      (if (not struct)
          (xml "")
          (let ([url           (struct->crud-url crudl:review struct)]
                [struct-pretty (render-struct-pretty seed struct)])
            (if url 
                (xml (a (@ [href ,url]) ,struct-pretty))
                (xml ,struct-pretty)))))
    
    ; Foreign keys are displayed as the pretty string, which takes the default pretty formatter here.
    ; seed snooze-struct -> xml
    (define/public (render-struct-pretty seed struct)
      (xml ,(format-snooze-struct struct)))))



; Sensible defaults for review-delete elements, which are essentially the same
(define (default-review+delete-mixin)
  (mixin/cells (crudl-review+delete+list<%> crud-element<%>)
    (crudl-review+delete<%>)
    
    (inherit get-struct
             get-attributes
             render-attributes
             render-attribute-label
             render-value)
    
    ; seed attribute any -> xml
    (define/override (render-attribute seed attr)
      (let ([value (snooze-struct-ref (get-struct) attr)])
        (xml (tr (th ,(render-attribute-label seed attr))
                 (td ,(render-value seed attr value))))))
    
    ; seed snooze-struct -> xml
    (define/override (render-struct seed struct)
      (xml (table (@ [class 'crud-review-delete])
                  (tbody ,(render-attributes seed (get-attributes))))))))



; Provides ---------------------------------------
(provide (all-defined-out))