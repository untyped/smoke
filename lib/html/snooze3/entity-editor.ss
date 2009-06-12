#lang scheme/base

(require (planet untyped/snooze:3)
         (planet untyped/unlib:3/symbol)
         "../../../lib-base.ss"
         "../html-element.ss"
         "attribute-editor.ss"
         "check-label.ss"
         "editor-interface.ss")

; Interfaces -------------------------------------

(define entity-editor<%>
  (interface (editor<%>)
    get-entity
    get-initial-value))

; Mixins -----------------------------------------

(define entity-editor-mixin
  (mixin/cells (html-element<%>) (entity-editor<%>)
    
    (inherit core-html-attributes)
    
    ; Fields -------------------------------------
    
    ; entity
    (init-field entity #:accessor)
    
    ; (listof attribute)
    (init [attributes (and entity (entity-data-attributes entity))])
    
    ; (listof attribute-editor<%>)
    (init-field editors
      (or (and attributes (map default-attribute-editor attributes))
          (error "entity-editor constructor: insufficient arguments"))
      #:accessor #:children)
    
    ; (U snooze-struct #f)
    (cell initial-value #f #:accessor)
    
    (init [classes null])
    
    (super-new [classes (list* 'smoke-entity-editor 'ui-widget classes)])
    
    ; Methods ------------------------------------
    
    ; seed -> xml
    (define/override (render seed)
      (xml (table (@ ,(core-html-attributes seed))
                  ,@(for/list ([editor (in-list (get-editors))])
                      (xml (tr (th (@ [class "attribute-label"])
                                   ,(send editor render-label seed))
                               (td ,(send editor render seed))))))))
    
    ; -> snooze-struct
    (define/public (get-value)
      (let ([init (get-initial-value)])
        (if (snooze-struct? init)
            (for/fold ([struct (get-initial-value)])
                      ([editor (in-list editors)])
                      (send editor restructure struct))
            (raise-type-error 'entity-editor.get-value "snooze-struct" #f))))
    
    ; snooze-struct -> void
    (define/public (set-value! struct)
      (unless (snooze-struct? struct)
        (raise-type-error 'entity-editor.set-value! "snooze-struct" struct))
      (web-cell-set! initial-value-cell struct)
      (for ([editor (in-list editors)])
        (send editor destructure! struct)))
    
    ; -> boolean
    (define/public (value-valid?)
      (not (check-failures? (check-snooze-struct (get-value)))))
    
    ; -> boolean
    (define/public (value-changed?)
      (for/or ([editor (in-list (get-editors))])
        (send editor value-changed?)))
    
    ; -> (listof check-result)
    (define/public (parse)
      (apply check-problems
             (for/list ([editor (in-list (get-editors))])
               (check/annotate ([ann:form-elements (list editor)])
                 (let ([message (with-handlers ([exn:smoke:form? exn-message])
                                  (send editor get-value)
                                  #f)])
                   (if message
                       (check-fail message)
                       (check-pass)))))))
    
    ; -> (listof check-result)
    (define/public (validate)
      (check-snooze-struct (get-value)))))

; Provide statements -----------------------------

(provide entity-editor<%>
         entity-editor-mixin)
