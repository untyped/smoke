#lang scheme/base

(require (planet untyped/snooze:3)
         (planet untyped/unlib:3/symbol)
         "../../../lib-base.ss"
         "../disableable-element.ss"
         "../html-element.ss"
         "../html-page.ss"
         "../notification.ss"
         "../submit-button.ss"
         "attribute-view.ss"
         "entity-view.ss"
         "page-internal.ss"
         "view-internal.ss")

; Mixins -----------------------------------------

(define entity-review-page-mixin
  (mixin/cells (html-element<%> html-page<%>) ()
    
    (inherit get-id)
    
    ; Fields ----------------------------
    
    (super-new)
    
    ; entity
    (init [entity #f])
    
    ; (listof attribute)
    (init [attributes (and entity (entity-data-attributes entity))])
    
    ; (listof attribute-view<%>)
    (init [views (and attributes (map default-attribute-view attributes))])
    
    ; entity-view%
    (init-field view
      (or (and entity
               views
               (new entity-view%
                    [id     (symbol-append (get-id) '-view)]
                    [entity entity]
                    [views  views]))
          (string-append "entity-review-page constructor: insufficient arguments"))
      #:child)
    
    ; Methods ---------------------------
    
    ; -> entity
    (define/public (get-entity)
      (send view get-entity))
    
    ; -> (U snooze-struct #f)
    (define/public (get-value)
      (send view get-value))
    
    ; snooze-struct -> void
    (define/public (set-value! struct)
      (send view set-value! struct))
    
    ; -> string
    (define/override (get-title)
      (let* ([title  (super get-title)]
             [entity (get-entity)]
             [struct (get-value)])
        (cond #;[title title]
              [struct (format-snooze-struct struct)]
              [else   (entity-pretty-name entity)])))
    
    ; seed -> xml
    (define/override (render seed)
      (send view render seed))))

; Procedures -------------------------------------

; entity [(subclassof html-page%)] -> html-page%
(define (scaffold-review-page entity [page% (default-scaffolded-page-superclass)])
  (new (entity-review-page-mixin (render-augride-mixin page%)) [entity entity]))

; Provide statements -----------------------------

(provide entity-review-page-mixin)

(provide/contract
 [scaffold-review-page (->* (entity?) ((subclass?/c html-page%)) (is-a?/c html-page%))])
