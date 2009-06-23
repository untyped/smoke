#lang scheme/base

(require (planet untyped/snooze:3)
         (planet untyped/unlib:3/symbol)
         "../../../lib-base.ss"
         "../disableable-element.ss"
         "../html-element.ss"
         "../html-page.ss"
         "../submit-button.ss"
         "attribute-editor.ss"
         "editor-controller.ss"
         "editor-interface.ss"
         "entity-editor.ss")

; Mixins -----------------------------------------

(define entity-editor-page-mixin
  (mixin/cells (html-element<%> html-page<%>) ()
    
    (inherit get-id)
    
    ; Fields ----------------------------
    
    (super-new)
    
    ; entity
    (init [entity #f])
    
    ; (listof attribute)
    (init [attributes (and entity (entity-data-attributes entity))])
    
    ; (listof attribute-editor<%>)
    (init [editors (and attributes (map default-attribute-editor attributes))])
    
    ; entity-editor%
    (init-field editor
      (or (and entity
               editors
               (new entity-editor%
                    [id      (symbol-append (get-id) '-editor)]
                    [entity  entity]
                    [editors editors]))
          (error "entity-editor-page constructor: insufficient arguments"))
      #:child #:accessor)
    
    ; submit-button%
    (field submit-button
      (new submit-button%
           [id     (symbol-append (get-id) '-submit)]
           [action (callback [editor on-update])])
      #:child)
    
    ; Methods ---------------------------
    
    ; -> entity
    (define/public (get-entity)
      (send editor get-entity))
    
    ; -> (U snooze-struct #f)
    (define/public (get-initial-struct)
      (send editor get-initial-struct))
    
    ; -> (U snooze-struct #f)
    (define/public (get-struct)
      (send editor get-struct))
    
    ; snooze-struct -> void
    (define/public (set-struct! struct)
      (send editor set-struct! struct))
    
    ; -> string
    (define/override (get-title)
      (let* ([title  (super get-title)]
             [entity (get-entity)]
             [struct (get-initial-struct)])
        (cond [title title]
              [(and struct (snooze-struct-saved? struct))
               (format "Edit ~a: ~a" (entity-pretty-name entity) (format-snooze-struct struct))]
              [struct (format "New ~a" (entity-pretty-name entity))]
              [else   (format "Editing ~a" (entity-pretty-name entity))])))
    
    ; seed -> xml
    (define/override (render seed)
      (xml ,(send editor render seed)
           ,(send submit-button render seed)))))

; Classes ----------------------------------------

(define entity-editor%
  (class/cells (editor-controller-mixin (entity-editor-mixin (simple-editor-mixin html-element%))) ()
    (inherit get-struct)
    ; -> void
    (define/override (commit-changes)
      (let ([struct (get-struct)])
        (call-with-transaction 
         #:metadata (list (if (snooze-struct-saved? struct)
                              (format "Created ~a" (format-snooze-struct struct))
                              (format "Updated ~a" (format-snooze-struct struct))))
         (lambda ()
           (begin0 (save! struct)
                   (clear-continuation-table!))))))))

; Provide statements -----------------------------

(provide entity-editor%
         entity-editor-page-mixin)
