#lang scheme/base

(require (planet untyped/snooze:3)
         (planet untyped/unlib:3/symbol)
         "../../../lib-base.ss"
         "../html-element.ss"
         "attribute-editor.ss"
         "check-label.ss"
         "editor-internal.ss")

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
    
    ; (listof editor<%>)
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
    
    ; (listof check-result) -> void
    (define/public (set-check-results! results)
      (for-each (cut send <> set-check-results! results)
                (get-editors)))
    
    ; -> snooze-struct
    (define/public (get-value)
      
      (let ([init (get-initial-value)])
        (if (snooze-struct? init)
            (for/fold ([val init])
                      ([editor (in-list (get-editors))])
                      (send editor restructure val))
            (raise-type-error 'entity-editor.get-value "snooze-struct" #f))))
    
    ; snooze-struct -> void
    (define/public (set-value! val)
      (unless (snooze-struct? val)
        (raise-type-error 'entity-editor.set-value! "snooze-struct" val))
      (web-cell-set! initial-value-cell val)
      (for ([editor (in-list (get-editors))])
        (send editor destructure! val)))
    
    ; -> boolean
    (define/public (value-changed?)
      (ormap (cut send <> value-changed?)
             (get-editors)))
    
    ; -> (listof check-result)
    (define/public (parse)
      (apply check-problems (map (cut send <> parse) (get-editors))))
    
    ; -> (listof check-result)
    (define/public (validate)
      (check-problems (check-snooze-struct (get-value))))
    
    ; -> void
    (define/public (commit-changes)
      (let ([val (get-value)])
        (call-with-transaction 
         #:metadata (list (if (snooze-struct-saved? val)
                              (format "Created ~a" (format-snooze-struct val))
                              (format "Updated ~a" (format-snooze-struct val))))
         (lambda ()
           (begin0 (save! val)
                   (clear-continuation-table!))))))))

; Classes ----------------------------------------

(define entity-editor%
  (entity-editor-mixin html-element%))

; Provide statements -----------------------------

(provide entity-editor<%>
         entity-editor-mixin
         entity-editor%)
