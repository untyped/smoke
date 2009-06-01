#lang scheme/base

(require srfi/19
         (planet untyped/snooze:3)
         (planet untyped/unlib:3/time)
         "../../../../lib-base.ss"
         "../../html-element.ss"
         "../editor.ss"
         "scaffold-common.ss")

; Interfaces -------------------------------------


; All CRUDL elements need access to an entity, its attributes, and the attribute-values of a snooze-struct
; Attribute-names and 
(define crudl-element<%>
  (interface (html-element<%>)
    get-entity                ; -> entity
    get-attributes            ; -> (listof attribute)
    get-attribute-pretty-name ; attr -> string
    get-struct-values         ; struct -> (listof any)
    render-struct             ; seed -> xml
    render-attribute-label))  ; seed attr -> xml

; Non-list elements deal only with a single struct
(define crud-element<%>
  (interface ()
    render-attribute  ; seed attribute -> xml
    render-attributes ; seed (listof attribute) -> xml
    set-struct!       ; snooze-struct -> void
    get-struct))      ; -> snooze-struct

; Editor elements inherit from snooze editor, and have a submit button label method
(define crudl-editor<%>
  (interface (snooze-editor<%>)
    get-button-label))

; All review-delete-list elements must:
;  - have access to the URLs of other CRUDL pages
;  - render an attribute, as plain or foreign-key variants
(define crudl-review+delete+list<%>
  (interface (crudl-element<%>)
    struct->crud-url         ; snooze-struct -> string
    render-value             ; seed snooze-struct attr -> xml. FINAL!
    render-value/plain       ; seed attr any -> xml
    render-value/foreign-key ; seed attr snooze-struct -> xml
    render-struct-pretty))   ; seed struct -> xml

; review and delete pages are essentially the same. They render a single struct with attribute labels and values.
(define crudl-review+delete<%>
  (interface (crudl-review+delete+list<%> crud-element<%>))) ; seed attribute value -> xml

; create and update pages need to convert attributes into editor components
(define crudl-create+update<%>
  (interface (crudl-element<%> crud-element<%> snooze-editor<%>)
    make-binder  ; attribute -> binder
    get-binder)) ; attribute -> binder

; list elements deal with a list of structs
(define crudl-list<%>
  (interface (crudl-review+delete+list<%>)
    entity->crudl-url?)) ; crudl-operation entity -> boolean

; list elements deal with a list of structs
(define crudl-static-list<%>
  (interface (crudl-list<%>)
    set-structs!  ; (listof snooze-struct) -> void
    get-structs)) ; -> (listof snooze-structs)

; list elements deal with a list of structs
(define crudl-report<%>
  (interface (crudl-list<%>)
    make-query)) ;


; Provides ---------------------------------------

(provide (all-from-out "scaffold-common.ss"))

(provide (all-defined-out))