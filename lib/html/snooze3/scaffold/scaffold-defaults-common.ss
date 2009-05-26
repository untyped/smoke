#lang scheme/base

(require (planet untyped/snooze:3)
         "../../../lib-base.ss"
         "../html-element.ss"
         "editor.ss"
         "scaffold-internal.ss")

; Mixins -----------------------------------------

; Sensible defaults for all CRUDL elements
(define (default-crudl-mixin entity)
  (mixin/cells (html-element<%>) (crudl-element<%>)
    
    ; -> entity
    (define/public-final (get-entity)
      entity)
    
    ; Skip the GUID and revision attributes, but retain all others
    ; -> (listof attribute)
    (define/public (get-entity-attrs)
      (cddr (entity-attributes entity)))
    
    ; Again, skip the GUID and revision values
    ; snooze-struct -> (listof any)
    (define/public (get-struct-values struct)
      (cddr (snooze-struct-ref* struct)))
    
    ; Take the default prettified attribute name, defined with the entity
    ; seed attribute -> xml
    (define/public (render-attr-name seed attr)
      (xml ,(attribute-pretty-name attr)))
    
    ; Contingent on the subclass, so do not define here.
    ; seed struct -> xml
    (define/public (render-struct seed struct)
      (error "render-struct must be overridden"))))



; Provide all single-struct CRUD pages with access to that struct
(define (default-crud-mixin)
  (mixin/cells (html-element<%>) (crud-element<%>)
    ; Fields -----------------------------------
    ; (cell (U snooze-struct #f))
    (init-cell struct #f #:accessor #:mutator)))



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
    (define/public-final (render-attr seed attr value)
      (let ([attr-type (attribute-type attr)])
        (if (and (guid-type? attr-type) value)
            (render-foreign-key-attr seed attr value)
            (render-plain-attr seed attr value))))
    
    ; Plain attributes are simply turned into strings and rendered.
    ; seed attribute any -> xml
    (define/public (render-plain-attr seed attr plain)
      (xml ,(format "~s" plain)))
    
    ; Foreign-keys are resolved to a URL if possible, and linked; otherwise they are just displayed.
    ; seed attribute snooze-struct -> xml
    (define/public-final (render-foreign-key-attr seed attr struct)
      (let ([url           (struct->crud-url crudl:review struct)]
            [struct-pretty (render-struct-pretty seed struct)])
        (if url 
            (xml (a (@ [href ,url]) ,struct-pretty))
            (xml ,struct-pretty))))
    
    ; Foreign keys are displayed as the pretty string, which takes the default pretty formatter here.
    ; seed snooze-struct -> xml
    (define/public (render-struct-pretty seed struct)
      (xml ,(format-snooze-struct struct)))))



; Sensible defaults for review-delete elements, which are essentially the same
(define (default-review+delete-mixin)
  (mixin/cells (crudl-element<%> crudl-review+delete+list<%>) (crudl-review+delete<%>)
    
    (inherit get-entity-attrs
             render-attr-name
             render-attr)
    
    ; seed attribute any -> xml
    (define/public (render-attr-name+value seed attr value)
      (xml (tr (th ,(render-attr-name seed attr))
               (td ,(render-attr seed attr value)))))
    
    ; seed snooze-struct -> xml
    (define/override (render-struct seed struct)
      (xml (table (@ [class 'crud-review-delete])
                  (tbody ,@(for/list ([attr (in-list (get-entity-attrs))])
                             (render-attr-name+value seed attr (snooze-struct-ref struct attr)))))))))


; Provides ---------------------------------------
(provide (all-defined-out))