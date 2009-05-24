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

; snooze-vanilla-combo-box%
(define snooze-foreign-key-combo-box%
  (class/cells snooze-vanilla-combo-box% ()
    
    (init-field entity #f #:accessor)
    
    ; -> (listof (cons integer string))
    (define/override (get-options)
      (let-alias ([E entity])
        (list* #f (select-all #:from E #:order ((asc E.guid))))))
    
    (define/override (option->raw option)
      (and option (guid? option) (snooze-struct-id option)))
    
    (define/override (raw->option raw)
      (and raw 
           (let ([id (string->number raw)])
             (and id (find-by-id entity id)))))
    
    (define/override (option->string option)
      (if option
          (format-snooze-struct option)
          (format "-- No ~a selected --" (entity-pretty-name entity))))))


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

; Editor elements inherit from snooze editor, and have a submit button label method
(define crudl-editor<%>
  (interface (snooze-editor<%>)
    get-button-label))

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
    render-attr-name+value)) ; seed attribute value -> xml

; create and update pages need to convert attributes into editor components
(define crudl-create+update<%>
  (interface (crudl-element<%> crud-element<%> snooze-editor<%>)
    make-editor               ; attribute -> form-element
    get-editor                ; attribute -> form-element
    render-attr-name+editor)) ; seed attribute editor -> xml

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

; Defaults for editor mixins
(define (crudl-editor-mixin)
  (let ([crudl-extension-mixin 
         (mixin/cells (snooze-editor<%>) (crudl-editor<%>)
           ; string -> submit-button
           (define/public (get-button-label)
             "Okay"))])
    (lambda (element) (crudl-extension-mixin (snooze-editor-mixin element)))))

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
  (mixin/cells (html-element<%> snooze-editor<%> crudl-element<%> crud-element<%> crudl-editor<%>)
    (crudl-create+update<%>)
    
    (inherit get-button-label
             get-struct
             get-entity-attrs
             render-attr-name)
    
    ; Fields -------------------------------------
    
    ; Hashmap used to provide a lookup from attribute to editor
    ; (hashof attribute form-element<%>)
    (field attr=>editor (make-hash) #:accessor)
    
    ; (listof form-element%)
    (field fields (init-fields) #:accessor #:children)
    
    ; submit-button%
    (field submit-button 
           (new submit-button% [action (callback on-update)] [label (get-button-label)]) 
           #:accessor #:child)
    
    ; Methods ------------------------------------
    
    ; (listof form-element<%>)
    (define/private (init-fields)
      (for/list ([attr (in-list (get-entity-attrs))])
        (let ([editor (make-editor attr)])
          (hash-set! attr=>editor attr editor)
          editor)))
          
    
    ; snooze-struct -> void
    (define/override (set-struct! struct)
      (super set-struct! struct)
      (for ([attr (in-list (get-entity-attrs))])
        (let ([val   (snooze-struct-ref struct attr)]
              [field (get-editor attr)])
          (send field set-value! 
                ; default types; otherwise convert to a string and show in a textfield
                (cond [(guid? val)     val]
                      [(boolean? val)  val]
                      [(integer? val)  val]
                      [(time-tai? val) (time-tai->date val)] ; TODO time/date fields
                      [(time-utc? val) (time-utc->date val)]
                      [else            (format "~a" val)])))))
    
    ; seed attribute any -> xml
    (define/public (make-editor attr)
      (let ([attr-type (attribute-type attr)])
        (cond [(guid-type? attr-type)
               (new snooze-foreign-key-combo-box%
                    [predicate (by-attributes attr)]
                    [entity    (guid-type-entity attr-type)])]
              [(boolean-type? attr-type)
               (new snooze-check-box% [predicate (by-attributes attr)])]
              [(integer-type? attr-type)
               (new snooze-integer-field% [predicate (by-attributes attr)])]
              [(or (time-tai-type? attr-type) (time-utc-type? attr-type))
               (new snooze-date-field% [predicate (by-attributes attr)])]
              [(and (string-type? attr-type) (or (not (character-type-max-length attr-type))
                                                 (> (character-type-max-length attr-type) 128)))
               (new snooze-text-area% [predicate (by-attributes attr)])]
              [else 
               (new snooze-text-field% [predicate (by-attributes attr)])])))
    
    ; attribute -> form-element<%>
    (define/public (get-editor attribute)
      (hash-ref attr=>editor attribute))
    
    ; seed attribute -> xml
    (define/public (render-attr-name+editor seed attr)
      (xml (tr (th ,(render-attr-name seed attr))
               (td ,(send (get-editor attr) render seed)))))
    
    ; seed snooze-struct -> xml
    (define/override (render-struct seed struct)
      (xml (table (@ [class 'crud-create-update])
                  (tbody ,@(for/list ([attr  (in-list (get-entity-attrs))])
                             (render-attr-name+editor seed attr))))
           ,(send submit-button render seed)))
    
    ; seed -> xml
    (define/augment (render seed)
      (render-struct seed (get-struct)))
    
    ; Validation ---------------------------------
    ; -> (listof check-result)
    (define/overment (validate)
      (super set-struct! 
             (apply snooze-struct-set 
                    (list* (get-struct)
                           (for/fold ([args null])
                             ([attr  (in-list (get-entity-attrs))]
                              [field (in-list fields)])
                             (list* attr (send field get-value) args)))))
      (inner (error "validate must be augmented") validate)) ; apply the validation procedure
    
    ; -> struct
    (define/override (commit-changes)
      (call-with-transaction
       (lambda () (save! (get-struct)))))))


; Defaults for review elements
(define (scaffold-review-mixin)
  (mixin/cells (html-element<%> crud-element<%> crudl-element<%> crudl-review+delete+list<%>) 
    (crudl-review+delete<%>)
    
    (inherit get-struct render-struct)
    
    ; seed -> xml
    (define/augment (render seed)
      (render-struct seed (get-struct)))))


; Sensible defaults for deleting a struct
(define (scaffold-delete-mixin)
  (mixin/cells (html-element<%> crud-element<%> crudl-element<%> crudl-review+delete+list<%> crudl-editor<%>) 
    (crudl-review+delete<%> crudl-editor<%> snooze-editor<%>)
    
    (inherit get-button-label
             get-struct
             render-struct)
    
    ; Fields -------------------------------------
    
    ; submit-button%
    (field delete-button 
           (new submit-button% [action (callback on-update)] [label (get-button-label)]) 
           #:accessor #:child)
    
    ; Methods ------------------------------------
    ; seed -> xml
    (define/augment (render seed)
      (xml ,(render-struct seed (get-struct))
           ,(send delete-button render seed)))
    
    ; Validation ---------------------------------
    ; -> (listof check-result)
    (define/override (validate)
      (error "validate must be overridden")) ; apply the validation procedure
    
    ; -> struct
    (define/override (commit-changes)
      (call-with-transaction
       (lambda () (delete! (get-struct)))))))




; Procedures -------------------------------------

;  entity
; ->
;  (mixinof html-element<%> -> crudl-review+delete<%>)
(define (scaffold-review-element entity
                                 #:crudl-mixin  [crudl-mixin  (scaffold-CRUDL-mixin entity)]
                                 #:crud-mixin   [crud-mixin   (scaffold-CRUD-mixin)]
                                 #:rdl-mixin    [rdl-mixin    (scaffold-RDL-mixin)]
                                 #:rd-mixin     [rd-mixin     (scaffold-RD-mixin)]
                                 #:review-mixin [review-mixin (scaffold-review-mixin)])
  (lambda (element)
    (review-mixin (rd-mixin (rdl-mixin (crud-mixin (crudl-mixin element)))))))

;  entity
; ->
;  (mixinof html-element<%> 
(define (scaffold-update-element entity
                                 #:crudl-mixin  [crudl-mixin  (scaffold-CRUDL-mixin entity)]
                                 #:crud-mixin   [crud-mixin   (scaffold-CRUD-mixin)]
                                 #:editor-mixin [editor-mixin (crudl-editor-mixin)]
                                 #:rdl-mixin    [rdl-mixin    (scaffold-RDL-mixin)]
                                 #:rd-mixin     [rd-mixin     (scaffold-RD-mixin)]
                                 #:update-mixin [update-mixin (scaffold-CU-mixin)])
  (lambda (element)
    (update-mixin (editor-mixin (rd-mixin (rdl-mixin (crud-mixin (crudl-mixin element))))))))

;  entity
; ->
;  (mixinof html-element<%> 
(define (scaffold-delete-element entity
                                 #:crudl-mixin  [crudl-mixin  (scaffold-CRUDL-mixin entity)]
                                 #:crud-mixin   [crud-mixin   (scaffold-CRUD-mixin)]
                                 #:editor-mixin [editor-mixin (crudl-editor-mixin)]
                                 #:rdl-mixin    [rdl-mixin    (scaffold-RDL-mixin)]
                                 #:rd-mixin     [rd-mixin     (scaffold-RD-mixin)]
                                 #:delete-mixin [delete-mixin (scaffold-delete-mixin)])
  (lambda (element)
    (delete-mixin (editor-mixin (rd-mixin (rdl-mixin (crud-mixin (crudl-mixin element))))))))


; Provides ---------------------------------------
(provide (except-out (all-defined-out)
                     snooze-foreign-key-combo-box%
                     crudl:create
                     crudl:review
                     crudl:update
                     crudl:delete
                     crudl:list))