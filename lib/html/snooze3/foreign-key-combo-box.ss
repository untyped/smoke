#lang scheme/base

(require (planet untyped/snooze:3)
         "../../../lib-base.ss"
         "form-element.ss")

; Components -------------------------------------

(define foreign-key-editor%
  (class/cells combo-box-editor% ()
    
    ; Fields -------------------------------------
    
    ; (U entity #f)
    (init-field entity #f #:accessor)
    
    ; Methods ------------------------------------
    
    ; -> (listof (cons integer string))
    (define/override (get-options)
      (let-sql ([entity entity])
        (list* #f (select-all #:from entity #:order ((asc entity.guid))))))
    
    ; (U snooze-struct #f) -> integer
    (define/override (option->raw option)
      (and option (guid? option) (snooze-struct-id option)))
    
    ; (U integer #f) -> snooze-struct
    (define/override (raw->option raw)
      (and raw 
           (let ([id (string->number raw)])
             (and id (find-by-id entity id)))))
    
    ; (U snooze-struct #f) -> string
    (define/override (option->string option)
      (if option
          (format-snooze-struct option)
          (format "-- No ~a selected --" (entity-pretty-name entity))))))

; Provides ---------------------------------------

(provide foreign-key-editor%)
