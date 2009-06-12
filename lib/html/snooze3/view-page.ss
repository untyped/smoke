#lang scheme/base

(require (planet untyped/snooze:3)
         (planet untyped/unlib:3/symbol)
         "../../../lib-base.ss"
         "../disableable-element.ss"
         "../html-element.ss"
         "../html-page.ss"
         "../submit-button.ss"
         "attribute-view.ss"
         "view-interface.ss"
         "entity-view.ss")

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
          (string-append "entity-view-page constructor: insufficient arguments"))
      #:child)
    
    ; Methods ---------------------------
    
    ; -> entity
    (define/public (get-entity)
      (send view get-entity))
    
    ; -> (U snooze-struct #f)
    (define/public (get-value)
      (send view get-value))
    
    ; snooze-struct -> void
    (define/public (set-value! val)
      (send view set-value! val))
    
    ; -> string
    (define/override (get-title)
      (let* ([title  (super get-title)]
             [entity (get-entity)]
             [struct (get-value)])
        (cond [title title]
              [struct (format "~a: ~a" (entity-pretty-name entity) (format-snooze-struct struct))]
              [else   (entity-pretty-name entity)])))
    
    ; seed -> xml
    (define/override (render seed)
      (send view render seed))))

(define entity-delete-page-mixin
  (mixin/cells (html-element<%> html-page<%>) ()
    
    (inherit get-id)
    
    ; Fields ----------------------------
    
    ; submit-button%
    (field submit-button
      (new submit-button%
           [action (callback on-delete)])
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
    (define/public (set-value! val)
      (send view set-value! val))
    
    ; -> string
    (define/override (get-title)
      (let* ([title  (super get-title)]
             [entity (get-entity)]
             [struct (get-value)])
        (cond [title title]
              [struct (format "Delete ~a: ~a" (entity-pretty-name entity) (format-snooze-struct struct))]
              [else   (format "Delete ~a" (entity-pretty-name entity))])))
    
    ; seed -> xml
    (define/override (render seed)
      (xml ,(send view render seed)
           ,(send submit-button render seed)))
    
    ; -> snooze-struct
    (define/public #:callback/return (on-delete)
      (delete! (get-value)))))

; Helpers ----------------------------------------

; component<%> -> symbol
(define (debug-id obj)
  (with-handlers ([exn? (lambda _ (send obj get-component-id))])
    (send obj get-id)))

; Provide statements -----------------------------

(provide entity-review-page-mixin
         entity-delete-page-mixin)
