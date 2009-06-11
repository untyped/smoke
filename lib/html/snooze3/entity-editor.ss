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
    get-entity))

; Mixins -----------------------------------------

(define entity-editor-mixin
  (mixin/cells (html-element<%>) (entity-editor<%>)
    
    (inherit core-html-attributes)
    
    ; Fields -------------------------------------
    
    ; entity
    (init-field entity #:accessor)
    
    ; (listof editor<%>)
    (init-field editors (default-entity-editors entity) #:accessor #:children)
    
    ; (U snooze-struct #f)
    (cell initial-value #f #:accessor)
    
    ; Methods ------------------------------------
    
    ; seed -> xml
    (define/override (render seed)
      (xml (table (@ ,(core-html-attributes seed))
                  ,@(for/list ([editor (in-list (get-editors))])
                      (xml (tr (th ,(send editor render-label seed))
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

; Helpers ----------------------------------------

; entity -> (listof attribute-editor<%>)
(define (default-entity-editors entity)
  (map default-attribute-editor (cddr (entity-attributes entity))))

; Provide statements -----------------------------

(provide entity-editor<%>
         entity-editor-mixin)

(provide/contract
 [default-entity-editors (-> entity? (listof (is-a?/c attribute-editor<%>)))])
