#lang scheme/base

(require (planet untyped/snooze:3)
         (planet untyped/unlib:3/enumeration)
         (planet untyped/unlib:3/symbol)
         "../../../lib-base.ss"
         "../combo-box.ss"
         "attribute-editor-internal.ss"
         "check-label.ss"
         "editor-interface.ss")

; Classes ----------------------------------------

(define foreign-key-editor%
  (class/cells (complete-attribute-editor-mixin vanilla-combo-box%) ()
    
    ; Fields -------------------------------------
    
    ; (U entity #f)
    (init-field entity #:accessor)
    
    ; (cell (U sql-expr #f))
    (init-cell where #f #:accessor #:mutator)
    
    ; (cell (listof sql-order))
    (init-cell order
      (let-sql ([entity (get-entity)])
        (sql-list (asc entity.guid)))
      #:accessor #:mutator)
    
    ; Methods ------------------------------------
    
    ; -> (listof (cons integer string))
    (define/override (get-options)
      (let-sql ([entity (get-entity)])
        (list* #f (select-all #:from  entity
                              #:where ,(get-where)
                              #:order ,(get-order)))))
    
    ; (U guid #f) -> (U string #f)
    (define/override (option->raw option)
      (and (guid? option)
           (number->string (snooze-struct-id option))))
    
    ; (U string #f) -> guid
    (define/override (raw->option raw)
      (and raw (let ([id (string->number raw)])
                 (and id (find-by-id entity id)))))
    
    ; (U snooze-struct #f) -> string
    (define/override (option->string option)
      (if option
          (format-snooze-struct option)
          (format "-- No ~a selected --" (entity-pretty-name entity))))))

; Provide statements -----------------------------

(provide foreign-key-editor%)
