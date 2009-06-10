#lang scheme/base

(require srfi/19
         (planet untyped/snooze:3)
         (planet untyped/unlib:3/time)
         "../../../../lib-base.ss"
         "../form-element.ss")

; Constants --------------------------------------

; symbol ...
(define crudl:create 'create)
(define crudl:review 'review)
(define crudl:update 'update)
(define crudl:delete 'delete)
(define crudl:list   'list)

; any -> boolean
(define crudl-operation/c
  (or/c crudl:create crudl:review crudl:update crudl:delete crudl:list))

; Structs ----------------------------------------

;  (listof attribute)
;  (listof form-element%)
;  struct -> void
;  -> (listof (cons attribute any))
(define-struct binder (attributes editors initialise! values) #:transparent)

; Some sensible defaults for attribute:editor binders
; - The attribute is stored as a single item ist (for compatibility with extended binders);
; - The editor is also stored in a list, as above.
; - The initialise procedure takes a struct, and finds the value for the specified attribute.
;   This is then set as the editor's value. ;A little type conversion is done here to constrain
;   the input to one of the default types (boolean, string, etc.).
; - The default values procedure returns a list of pairs, each containing the attribute to set
;   and the new value to associate with that attribute. This is simply a cons of the attribute 
;   and the current value from the editor, in the default case.
;
; attribute editor -> binder
(define (make-default-binder attribute editor)
  (make-binder (list attribute) ; The default list of attributes
               (list editor)    ; The default list of editors
               (lambda (struct) ; The default binder-initialise procedure for setting editor values
                 (let* ([val   (snooze-struct-ref struct attribute)]
                        ; default types; otherwise convert to a string and show in a textfield
                        [value (cond [(or (guid? val) (boolean? val) (integer? val))
                                      val]
                                     [(time-tai? val)
                                      (time-tai->date val)] ; TODO time/date fields
                                     [(time-utc? val)
                                      (time-utc->date val)]
                                     [else
                                      (format "~a" val)])])
                   (send editor set-value! value)))
               (lambda ()       ; The default binder-values procedure, returning edits by attribute.
                 (let* ([val   (send editor get-value)]
                        [type  (attribute-type attribute)]
                        [value (cond [(and val (symbol-type? type))
                                      (string->symbol val)]
                                     [(and val (time-utc-type? type))
                                      (date->time-utc val)]
                                     [(and val (time-tai-type? type))
                                      (date->time-tai val)]
                                     [else val])])
                   
                   (list (cons attribute value))))))

; Provides ---------------------------------------

(provide crudl-operation/c)

(provide/contract 
 [crudl:create 'create] 
 [crudl:review 'review] 
 [crudl:update 'update] 
 [crudl:delete 'delete] 
 [crudl:list   'list]
 [struct binder ([attributes  (listof attribute?)]
                 [editors     (listof (is-a?/c snooze-form-element<%>))]
                 [initialise! (-> snooze-struct? void)]
                 [values      (-> (listof (cons/c attribute? any/c)))])]
 [make-default-binder (-> attribute?
                          (is-a?/c snooze-form-element<%>)
                          binder?)])
