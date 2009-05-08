#lang scheme

(require srfi/19
         (planet untyped/snooze:2)
         "../../../lib-base.ss"
         "../submit-button.ss"
         "../html-element.ss"
         "editor.ss"
         "form-element.ss")

; Procedures -------------------------------------

; entity -> (listof attribute)
(define (default-attributes entity)
  (cddr (entity-attributes entity)))

; persistent-struct -> (listof any)
(define (default-struct-ref* struct)
  (cddr (struct-attributes struct)))

; Interfaces -------------------------------------

; A really simple html-element creation mixin:
; mixin against any html-element and get to specify the XML as an init argument.
(define vanilla-element<%>
  (interface ()
    get-inner-xml    ; -> (listof snooze-form-element%)
    set-inner-xml!)) ; persistent-struct -> void

; The basis for scaffolding - pages must have a struct
(define snooze-scaffolded-element<%>
  (interface ()
    set-struct!  ; persistent-struct -> void 
    get-struct)) ; -> persistent-struct

; Editors add a list of fields, and must also subtype snooze-editor<%>
(define snooze-scaffolded-editor<%>
  (interface (snooze-scaffolded-element<%> snooze-editor<%>)
    get-fields))   ; -> (listof snooze-form-element%)

; The basis for list scaffolding - requires a list of structs
(define snooze-scaffolded-list-element<%>
  (interface ()
    set-structs!  ; (listof persistent-struct) -> void 
    get-structs)) ; -> (listof persistent-struct)


; Mixins -----------------------------------------
(define vanilla-element-mixin
  (mixin/cells (html-element<%>) (vanilla-element<%> html-element<%>)             
    ; xml
    (init-cell inner-xml #f #:accessor #:mutator)
    ; seed -> xml
    (define/augment (render seed)
      (get-inner-xml))))


; Review pages -----------------------------------

; A mixin for a particular entity review page.
;
;  entity
;  [(attr -> (value -> xml))]
;  [#:attributes  (listof attribute)]
;  [#:struct-ref* (persistent-struct -> (listof any))]
; -> 
;  (mixin html-element<%>)
(define (entity->review-mixin entity 
                              [attr->review-renderer default-attr->review-renderer]
                              #:attributes [attributes (default-attributes entity)])
  (mixin/cells (html-element<%>) (snooze-scaffolded-element<%>)
    
    ; Fields -----------------------------------
    ; (cell (U snooze-struct #f))
    (init-cell struct #f #:accessor #:mutator)
    
    ; (struct -> xml)
    (field renderer 
           (entity->review-renderer entity attr->review-renderer #:attributes attributes)
           #:accessor)
    
    ; Methods ------------------------------------ 
    ; seed -> xml
    (define/augment (render seed)
      (renderer (get-struct)))))

; Generates a procedure that takes persistent-structs for a particular entity
; and returns an appropriate XML representation.
; Uses the default attribute-renderer, but that may be replaced.
;
; entity [(attr -> (value -> xml))] -> 
;  entity
;  [(attr -> (value -> xml))]
;  [#:attributes  (listof attribute)]
; -> 
;  (struct -> xml)
(define (entity->review-renderer entity 
                                 [attr->review-renderer default-attr->review-renderer]
                                 #:attributes   [attributes (default-attributes entity)])
  (lambda (struct)
    (xml (dl (@ [class 'snooze-review-entity])
             ,@(for/list ([attr (in-list attributes)])
                 ((attr->review-renderer attr) (struct-attribute struct attr)))))))

; The default attribute-value renderer.
;
; attr -> (value -> xml)
(define (default-attr->review-renderer attr)
  (lambda (value)
    (xml (dt (@ [class 'snooze-review-attr])  ,(attribute-name attr))
         (dd (@ [class 'snooze-review-value]) ,value))))

; List pages -------------------------------------

; A mixin for a particular entity review page.
;
;  entity
;  [(attr -> (value -> xml))]
;  [#:attributes  (listof attribute)]
;  [#:struct-ref* (persistent-struct -> (listof any))]
; -> 
;  (mixin html-element<%>)
(define (entity->list-mixin entity 
                              [attr->review-renderer default-attr->list-renderer]
                              #:entity->attrs [entity->attrs default-attributes])
  (mixin/cells (html-element<%>) (snooze-scaffolded-list-element<%>)
    
    ; Fields -----------------------------------
    ; (cell (listof snooze-struct))
    (init-cell structs null #:accessor #:mutator)
    
    ; (struct -> xml)
    (field renderer 
           (entity->list-renderer entity attr->review-renderer #:entity->attrs entity->attrs)
           #:accessor)
    
    ; Methods ------------------------------------ 
    ; seed -> xml
    (define/augment (render seed)
      (renderer (get-structs)))))

;  entity
;  [(attr -> (value -> xml))]
;  [#:entity->attrs (entity -> (listof attribute))]
; ->
;  ((listof struct) -> xml)
(define (entity->list-renderer entity 
                               [attr->list-renderer default-attr->list-renderer]
                               #:entity->attrs [entity->attrs default-attributes])
  (let* ([attributes (entity->attrs entity)])
    (lambda (structs)
      (xml (table (@ [class 'snooze-list])
                  (thead (tr ,@(for/list ([attr (in-list attributes)])
                                 (xml (th ,(attribute-name attr))))))
                  (tbody ,@(for/list ([struct (in-list structs)])
                             (struct->list-xml struct attr->list-renderer entity->attrs))))))))

; snooze-struct (attr -> (value -> xml)) -> xml
(define (struct->list-xml struct attr->renderer entity->attrs)
  (let* ([entity     (struct-entity struct)]
         [attributes (entity->attrs entity)])
    (xml (tr ,@(for/list ([attr (in-list attributes)])
                 ((attr->renderer attr) (struct-attribute struct attr)))))))

; attr -> (value -> xml)
(define (default-attr->list-renderer attr)
  (lambda (value)
    (xml (td ,value))))


; Editors ----------------------------------------

; Generates a mixin that acts as an editor element for persistent-structs of a given entity type.
;
; By default, all attributes except persistent-struct-id and persistent-struct-revision are editable.
; This behaviour may be changed by providing the list of attributes required using #:attributes.
; Similarly, the #:struct-ref* procedure retrieves the list of values for a given struct. This defaults
; to all values except id and revision.
;
; Editors are inferred using the attribute->editor procedure, which defaults to default-attr->editor.
; This behaviour may be changed by specifying a new attribute->editor procedure using #:attr->editor.
;
; Prior to saving, the persistent-struct is validated using the #:check-proc procedure.
; By default, no validation is performed; #:check-proc should be replaced with a genuine validation procedure.
;
;  snooze
;  entity
;  [#:attributes   (listof attribute)]
;  [#:struct-ref*  (persistent-struct -> (listof any))]
;  [#:attr->editor (attr -> snooze-form-element<%>)]
;  [#:check-proc   (persistent-struct -> (listof check-result))]
; ->
;  snooze-editor-mixin
(define (entity->editor-mixin snooze entity
                              #:attributes   [attributes   (default-attributes entity)]
                              #:struct-ref*  [struct-ref*  default-struct-ref*]
                              #:attr->editor [attr->editor default-attr->editor] 
                              #:check-proc   [check-proc   (lambda (struct) null)])
  (define-snooze-interface snooze) ; required for aliases for save!, call-with-transaction, etc.
  ; (snooze-editor% -> snooze-scaffolded-editor<%>)
  (define scaffolded-mixin
    (mixin/cells (snooze-editor<%> html-element<%>) (snooze-scaffolded-editor<%>)
      
      ; Fields -----------------------------------
      ; (cell (U snooze-struct #f))
      (init-cell struct #f #:accessor)
      
      ; (listof form-element%)
      (field fields
             (for/list ([attr (in-list attributes)])
               (attr->editor attr))
             #:accessor
             #:children)
      
      ; submit-button%
      (field submit-button
             (new submit-button%
                  [action (callback on-update)]
                  [label  "Okay"])
             #:accessor
             #:child)
      
      ; Methods ----------------------------------
      
      ; snooze-struct -> void
      (define/public (set-struct! struct)
        (web-cell-set! struct-cell struct)
        (for ([val   (in-list (struct-ref* struct))]
              [field (in-list fields)])
          (send field set-value! 
                ; default types; otherwise convert to a string and show in a textfield
                (cond [(boolean? val)  val]
                      [(integer? val)  val]
                      [(time-tai? val) (time-tai->date val)]
                      [(time-utc? val) (time-utc->date val)]
                      [else            (format "~a" val)]))))
      
      ; seed -> xml
      (define/augment (render seed)
        (xml (table (tbody ,@(for/list ([field (in-list fields)]
                                        [attr  (in-list attributes)])
                               (xml (tr (th ,(attribute-name attr))
                                        (td ,(send field render seed)))))))
             ,(send submit-button render seed)))
      
      ; -> (listof check-result)
      (define/override (validate)
        (web-cell-set! 
         struct-cell 
         (apply copy-persistent-struct 
                (list* (get-struct)
                       (for/fold ([args null])
                         ([attr  (in-list attributes)]
                          [field (in-list fields)])
                         (list* attr (send field get-value) args)))))
        (check-proc (get-struct))) ; apply the validation procedure
      
      ; -> struct
      (define/override (commit-changes)
        (let* ([struct (get-struct)]
               [entity (struct-entity struct)])
          (call-with-transaction
           (lambda () (save! struct))
           (if (struct-saved? struct)
               (format "Edit ~a details: ~a" (entity-name entity) struct)
               (format "Create ~a: ~a" (entity-name entity) struct)))))))
  ; Create a compound mixin of the above and snooze-editor-mixin
  (lambda (html-element) 
    (scaffolded-mixin (snooze-editor-mixin html-element))))

; The default attribute->editor procedure
;
; attr -> snooze-form-element<%>
(define (default-attr->editor attr)
  (cond [(boolean-type? (attribute-type attr))
         (new snooze-check-box% [predicate (by-attributes attr)])]
        [(integer-type? (attribute-type attr))
         (new snooze-integer-field% [predicate (by-attributes attr)])]
        [(or (time-tai-type? (attribute-type attr)) (time-utc-type? (attribute-type attr)))
         (new snooze-date-field% [predicate (by-attributes attr)])]
        [else 
         (new snooze-text-field% [predicate (by-attributes attr)])]))

; Provides ---------------------------------------

(provide (all-defined-out))