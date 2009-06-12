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
    get-struct
    set-struct!
    get-initial-struct))

; Mixins -----------------------------------------

(define entity-editor-mixin
  (mixin/cells (html-element<%> editor<%>) (entity-editor<%>)
    
    (inherit core-html-attributes
             get-editors)
    
    ; Fields -------------------------------------
    
    ; entity
    (init-field entity #:accessor)
    
    ; (listof attribute)
    (init [attributes (and entity (entity-data-attributes entity))])
    
    ; (listof attribute-editor<%>)
    (init [editors    (or (and attributes (map default-attribute-editor attributes))
                          (error "entity-editor constructor: insufficient arguments"))])
    
    ; (U snooze-struct #f)
    (cell initial-struct #f #:accessor)
    
    (init [classes null])
    
    (super-new [classes (list* 'smoke-entity-editor 'ui-widget classes)] [editors editors])
    
    ; Methods ------------------------------------
    
    ; seed -> xml
    (define/override (render seed)
      (xml (table (@ ,(core-html-attributes seed))
                  ,@(for/list ([editor (in-list (get-editors))])
                      (xml (tr (th (@ [class "attribute-label"])
                                   ,(send editor render-label seed))
                               (td ,(send editor render seed))))))))
    
    ; -> snooze-struct
    (define/public (get-struct)
      (let ([init (get-initial-struct)])
        (if (snooze-struct? init)
            (for/fold ([struct init])
                      ([editor (in-list (get-editors))])
                      (send editor restructure struct))
            (raise-type-error 'entity-editor.get-struct "snooze-struct" #f))))
    
    ; snooze-struct -> void
    (define/public (set-struct! struct)
      (unless (snooze-struct? struct)
        (raise-type-error 'entity-editor.set-struct! "snooze-struct" struct))
      (web-cell-set! initial-struct-cell struct)
      (for ([editor (in-list (get-editors))])
        (send editor destructure! struct)))
        
    ; -> (listof check-result)
    (define/override (validate)
      (check-problems
       (super validate)
       (check-snooze-struct (get-struct))))))

; Provide statements -----------------------------

(provide entity-editor<%>
         entity-editor-mixin)
