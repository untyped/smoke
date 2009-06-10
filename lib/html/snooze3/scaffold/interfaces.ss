#lang scheme/base

(require srfi/19
         (planet untyped/snooze:3)
         (planet untyped/unlib:3/time)
         "../../../../lib-base.ss"
         "../../html-element.ss"
         "../editor.ss"
         "scaffold-common.ss")

; Interfaces -------------------------------------

; DJG : The names of the interfaces are a bit unintuitive.

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

; DJG : Can render-attribute[s] be moved into crudl-element<%>? That way, we have all attribute rendering stuff in one interface.

; Non-list elements deal only with a single struct
(define crud-element<%>
  (interface ()
    render-attribute  ; seed attribute -> xml          ; render the label (and the value)
    render-attributes ; seed (listof attribute) -> xml ; render the labels (and the values)
    set-struct!       ; snooze-struct -> void
    get-struct))      ; -> snooze-struct

; DJG : Roll this into snooze-editor<%>?

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

; DJG : Binders = macro form elements ??

; create and update pages need to convert attributes into editor components
(define crudl-create+update<%>
  (interface (crudl-element<%> crud-element<%> snooze-editor<%>)
    make-binder/attrs  ; attribute -> binder
    get-binder))       ; attribute -> binder

; DJG : I'd prefer get-create-controller, get-review-controller, get-update-controller, etc...

; list elements deal with a list of structs
(define crudl-list<%>
  (interface (crudl-review+delete+list<%>)
    entity->crudl-url?)) ; crudl-operation entity -> boolean

; list elements deal with a list of structs
(define crudl-static-list<%>
  (interface (crudl-list<%>)
    set-structs!  ; (listof snooze-struct) -> void
    get-structs)) ; -> (listof snooze-structs)

; DJG : make-query is already sort-of defined in snooze-report<%> (in that it's possible to override the parts of the queries).
; DJG : a good method here would be get-entity-alias.

; list elements deal with a list of structs
(define crudl-report<%>
  (interface (crudl-list<%>)
    make-query      ;
    make-columns    ;
    make-filters    ;
    make-views      ;
    render-column)) ;


; Provides ---------------------------------------

(provide (all-from-out "scaffold-common.ss"))

(provide (all-defined-out))