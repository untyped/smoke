#lang scheme/base

(require (planet untyped/snooze:3)
         (planet untyped/unlib:3/symbol)
         "../../../lib-base.ss"
         "../html-element.ss"
         "attribute-view.ss"
         "check-label.ss"
         "view-interface.ss")

; Interfaces -------------------------------------

(define entity-view<%>
  (interface (view<%>)
    get-entity)) ; -> entity

; Mixins -----------------------------------------

(define entity-view-mixin
  (mixin/cells (html-element<%>) (entity-view<%>)
    
    (inherit core-html-attributes)
    
    ; Fields -------------------------------------
    
    ; entity
    (init-field entity #:accessor)
    
    ; (listof attribute)
    (init [attributes (and entity (entity-attributes entity))])
    
    ; (listof attribute-view<%>)
    (init-field views
      (or (and attributes (map default-attribute-view attributes))
          (error "entity-editor constructor: insufficient arguments"))
      #:accessor #:children)
    
    ; (cell (U snooze-struct #f))
    (cell value #f #:accessor)
        
    (init [classes null])
    
    (super-new [classes (list* 'smoke-entity-view 'ui-widget classes)])

    ; Methods ------------------------------------
    
    ; seed -> xml
    (define/override (render seed)
      (render-value seed))
    
    ; seed -> xml
    (define/public (render-value seed)
      (xml (table (@ ,(core-html-attributes seed))
                  ,@(for/list ([view (in-list (get-views))])
                      (xml (tr (th (@ [class "attribute-label"])
                                   ,(send view render-label seed))
                               (td ,(send view render seed))))))))
    
    ; snooze-struct -> void
    (define/public (set-value! struct)
      (unless (snooze-struct? struct)
        (raise-type-error 'entity-view.set-value! "snooze-struct" struct))
      (web-cell-set! value-cell struct)
      (for ([view (in-list views)])
        (send view destructure! struct)))))

(define entity-view%
  (entity-view-mixin html-element%))

; Provide statements -----------------------------

(provide entity-view<%>
         entity-view-mixin
         entity-view%)
