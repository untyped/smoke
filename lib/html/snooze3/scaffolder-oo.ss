#lang scheme

(require srfi/13
         srfi/19
         (only-in (planet untyped/unlib:3/list) assemble-list)
         (planet untyped/unlib:3/time)
         (planet untyped/snooze:3)
         "../../../lib-base.ss"
         "../submit-button.ss"
         "../html-element.ss"
         "editor.ss"
         "report.ss"
         "form-element.ss")




; Constants --------------------------------------

; symbol ...
(define crudl:create 'create)
(define crudl:review 'review)
(define crudl:update 'update)
(define crudl:delete 'delete)
(define crudl:list   'list)


; Interfaces -------------------------------------

; All CRUDL elements need access to an entity, its attributes, and the attribute-values of a snooze-struct
; Attribute-names and 
(define crudl-element<%>
  (interface ()
    get-entity        ; -> entity
    get-entity-attrs  ; -> (listof attribute)
    get-struct-values ; struct -> (listof any)
    render-attr-name  ; snooze-seed attr -> xml
    render-struct))   ; seed snooze-struct -> xml

; Non-list elements deal only with a single struct
(define crud-element<%>
  (interface ()
    set-struct!  ; snooze-struct -> void
    get-struct)) ; -> snooze-struct

; All review-delete-list elements must:
;  - have access to the URLs of other CRUDL pages
;  - render an attribute, as plain or foreign-key variants
(define crudl-review+delete+list<%>
  (interface (crudl-element<%>)
    struct->crud-url        ; snooze-struct -> string
    render-attr             ; seed snooze-struct attr -> xml. FINAL!
    render-plain-attr       ; seed attr any -> xml
    render-foreign-key-attr ; seed attr snooze-struct -> xml
    render-struct-pretty))  ; seed struct -> xml

; review and delete pages are essentially the same. They render a single struct with attribute labels and values.
(define crudl-review+delete<%>
  (interface (crudl-review+delete+list<%> crud-element<%>)
    render-attr-name+value)) ; seed 

; create and update pages need to convert attributes into editor components
(define crudl-create+update<%>
  (interface (crudl-element<%> crud-element<%>)
    get-editor-component)) ; attribute-type -> form-element

; list elements deal with a list of structs
(define crudl-list<%>
  (interface (crudl-review+delete+list<%>)
    set-structs!  ; (listof snooze-struct) -> void
    get-structs)) ; -> (listof snooze-structs)



; Mixins -----------------------------------------

; Sensible defaults for all CRUDL elements
(define (scaffold-CRUDL-mixin entity)
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
(define (scaffold-CRUD-mixin)
  (mixin/cells (html-element<%>) (crud-element<%>)
    ; Fields -----------------------------------
    ; (cell (U snooze-struct #f))
    (init-cell struct #f #:accessor #:mutator)))

; Defaults for review-delete-list pages (the display-only pages).
(define (scaffold-RDL-mixin)
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
(define (scaffold-RD-mixin)
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


(define (scaffold-CU-mixin)
  (mixin/cells (crudl-element<%> crudl-review+delete+list<%>) (crudl-create+update<%>)
    
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


; Defaults for review elements
(define (scaffold-review-mixin)
  (mixin/cells (html-element<%> crud-element<%> crudl-element<%> crudl-review+delete+list<%>) 
    (crudl-review+delete<%>)
    
    (inherit get-struct render-struct)
    
    ; seed -> xml
    (define/augment (render seed)
      (render-struct seed (get-struct)))))

(define (scaffold-editor-mixin)
  (mixin/cells (html-element<%> crud-element<%> crudl-element<%> crudl-review+delete+list<%>) 
    (crudl-review+delete<%>)
    
    (inherit get-struct render-struct)
    
    ; Fields -------------------------------------
    
    
    ; Methods ------------------------------------
    ; seed -> xml
    (define/augment (render seed)
      (render-struct seed (get-struct)))))




; Procedures -------------------------------------

;  entity
; ->
;  (mixinof html-element<%> 
(define (scaffold-review-element entity
                                 #:crudl-mixin  [crudl-mixin  (scaffold-CRUDL-mixin entity)]
                                 #:crud-mixin   [crud-mixin   (scaffold-CRUD-mixin)]
                                 #:rdl-mixin    [rdl-mixin    (scaffold-RDL-mixin)]
                                 #:rd-mixin     [rd-mixin     (scaffold-RD-mixin)]
                                 #:review-mixin [review-mixin (scaffold-review-mixin)])
  (lambda (element)
    (review-mixin (rd-mixin (rdl-mixin (crud-mixin (crudl-mixin element)))))))

; Provides ---------------------------------------
(provide (except-out (all-defined-out)
                     crudl:create
                     crudl:review
                     crudl:update
                     crudl:delete
                     crudl:list))