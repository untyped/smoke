#lang scheme/base

(require (planet untyped/snooze:3)
         (planet untyped/unlib:3/symbol)
         "../../../lib-base.ss"
         "../html-element.ss"
         "attribute-view.ss"
         "check-label.ss"
         "view-internal.ss")

; Interfaces -------------------------------------

(define entity-view<%>
  (interface (view<%>)
    get-entity)) ; -> entity

; Mixins -----------------------------------------

(define entity-view-mixin
  (mixin/cells (html-element<%> view<%>) (entity-view<%>)
    
    (inherit core-html-attributes
             get-views)
    
    ; Fields -------------------------------------
    
    ; entity
    (init-field entity #:accessor)
    
    ; (listof attribute)
    (init [attributes (and entity (entity-data-attributes entity))])
    
    ; (listof attribute-view<%>)
    (init [views (or (and attributes (map default-attribute-view attributes))
                     (error "entity-view constructor: insufficient arguments"))])
    
    (init [classes null])
    
    (super-new [classes (list* 'smoke-entity-view 'ui-widget classes)] [views views])

    ; Methods ------------------------------------
    
    ; seed -> xml
    (define/override (render seed)
      (xml (table (@ ,(core-html-attributes seed))
                  ,@(for/list ([view (in-list (get-views))])
                      (xml (tr (th (@ [class "attribute-label"])
                                   ,(send view render-label seed))
                               (td ,(send view render seed))))))))
    
    ; snooze-struct -> void
    (define/override (set-value! val)
      (unless (snooze-struct? val)
        (raise-type-error 'entity-view.set-value! "snooze-struct" val))
      (super set-value! val)
      (for ([view (in-list (get-views))])
        (send view destructure! val)))))

(define entity-view%
  (entity-view-mixin (simple-view-mixin html-element%)))

; Provide statements -----------------------------

(provide entity-view<%>
         entity-view-mixin
         entity-view%)
