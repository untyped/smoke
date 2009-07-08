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

(define entity-delete-page-mixin
  (mixin/cells (html-element<%> html-page<%>) ()
    
    (inherit get-id)
    
    ; Fields ----------------------------
    
    ; submit-button%
    (field submit-button
      (new submit-button%
           [action (callback on-delete)]
           [label  "Delete"])
      #:child)
    
    (super-new)
    
    ; entity
    (init entity)
    
    ; (listof attribute)
    (init [attributes (and entity (entity-attributes entity))])
    
    ; (listof attribute-view<%>)
    (init [views (and attributes (map default-attribute-view attributes))])
    
    ; entity-view%
    (init-field view
      (new entity-view%
           [id     (symbol-append (get-id) '-view)]
           [entity entity]
           [views  views])
      #:child)
    
    ; Methods ---------------------------
    
    ; -> entity
    (define/public (get-entity)
      (send view get-entity))
    
    ; -> (U snooze-struct #f)
    (define/public (get-value)
      (send view get-value))
    
    ; (U snooze-struct #f) -> void
    (define/public (set-value! struct)
      (send view set-value! struct))
    
    ; -> string
    (define/override (get-title)
      (let* ([title  (super get-title)]
             [entity (get-entity)]
             [struct (get-value)])
        (cond #;[title title]
              [struct (format "Delete ~a: ~a" (entity-pretty-name entity) (format-snooze-struct struct))]
              [else   (format "Delete ~a" (entity-pretty-name entity))])))
    
    ; seed -> xml
    (define/override (render seed)
      (xml ,(render-confirmation-message seed)
           ,(send view render seed)
           ,(send submit-button render seed)))
    
    ; seed -> xml
    (define/public (render-confirmation-message seed)
      (xml "You are about to delete the " ,(entity-name (get-entity)) " shown below. "
           "There is no way of undoing this operation. "
           "Click " ,(format "~s" (send submit-button get-label))" at the bottom of the page "
           "if you wish to proceed."))
    
    ; snooze-struct -> xml
    (define/public (get-delete-notification struct)
      (xml "Successfully deleted " ,(entity-pretty-name (snooze-struct-entity struct)) ": "
           ,(format-snooze-struct struct) "."))
    
    ; -> snooze-struct
    (define/public #:callback/return (on-delete)
      (let ([struct (get-value)])
        (call-with-transaction
         #:metadata (list (format "Delete ~a" (format-snooze-struct struct)))
         (lambda ()
           (begin0
             (delete! struct)
             (clear-continuation-table!)
             (notifications-add! (get-delete-notification struct)))))))))

; Procedures -------------------------------------

; entity [(subclassof html-page%)] -> html-page%
(define (scaffold-delete-page entity [page% (default-scaffolded-page-superclass)])
  (new (entity-delete-page-mixin (render-augride-mixin page%)) [entity entity]))

; Provide statements -----------------------------

(provide entity-delete-page-mixin)

(provide/contract
 [scaffold-delete-page (->* (entity?) ((subclass?/c html-page%)) (is-a?/c html-page%))])
