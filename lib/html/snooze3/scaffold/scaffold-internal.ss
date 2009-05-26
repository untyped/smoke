#lang scheme/base

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

; any -> boolean
(define (crudl-operation/c arg)
  (or/c crudl:create crudl:review crudl:update crudl:delete crudl:list))

; Interfaces -------------------------------------

; All CRUDL elements need access to an entity, its attributes, and the attribute-values of a snooze-struct
; Attribute-names and 
(define crudl-element<%>
  (interface ()
    get-entity         ; -> entity
    get-entity-attrs   ; -> (listof attribute)
    get-struct-values  ; struct -> (listof any)
    render-attr-name)) ; snooze-seed attr -> xml

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

; reports rely on query construction
(define crudl-report<%>
  (interface (crudl-review+delete+list<%>)
    set-make-query!  
    get-make-query))


; Provides ---------------------------------------

(provide (all-defined-out))