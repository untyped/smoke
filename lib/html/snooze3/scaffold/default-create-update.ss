#lang scheme/base

(require srfi/19
         (planet untyped/unlib:3/time)
         (planet untyped/snooze:3)
         "../../../../lib-base.ss"
         "../../submit-button.ss"
         "../../html-element.ss"
         "../editor.ss"
         "../foreign-key-combo-box.ss"
         "../form-element.ss"
         "scaffold-internal.ss")

; Mixins -----------------------------------------

(define (default-create+update-mixin)
  (mixin/cells (html-element<%> snooze-editor<%> crudl-element<%> crud-element<%> crudl-editor<%>)
    (crudl-create+update<%>)
    
    (inherit get-button-label
             get-struct
             get-attributes
             render-attribute-label
             render-attributes)
    
    ; Fields -------------------------------------
    
    ; Hashmap used to provide a lookup from attribute to editor
    ; (hashof attribute form-element<%>)
    (field attr=>binder (make-hash) #:accessor)
    
    ; (listof form-element%)
    (field fields (init-fields) #:accessor #:children)
    
    ; submit-button%
    (field submit-button 
           (new submit-button% [action (callback on-update)] [label (get-button-label)]) 
           #:accessor #:child)
    
    ; Methods ------------------------------------
    
    ; Creates binders for each attribute, and caches in a lookup for quick retrieval.
    ;
    ; (listof form-element<%>)
    (define/private (init-fields)
      (flatten (for/list ([attr (in-list (get-attributes))])
                 (let ([binder (make-binder attr)])
                   (hash-set! attr=>binder attr binder)
                   (binder-editors binder)))))
    
    ; snooze-struct -> void
    (define/override (set-struct! struct)
      (super set-struct! struct)
      ; get each bound editor, and initialise values from struct
      (for ([attr (in-list (get-attributes))])
        (let ([binder (get-binder attr)])
          ((binder-initialise! binder) struct))))   
    
    ; (U attribute (listof attribute)) -> binder
    (define/public (make-binder attr)
      (let ([attr-type (attribute-type attr)])
        (cond [(guid-type? attr-type)
               (make-default-binder attr (new snooze-foreign-key-combo-box%
                                              [predicate (by-attributes attr)]
                                              [entity    (guid-type-entity attr-type)]))]
              [(boolean-type? attr-type)
               (make-default-binder attr (new snooze-check-box% [predicate (by-attributes attr)]))]
              [(integer-type? attr-type)
               (make-default-binder attr (new snooze-integer-field% [predicate (by-attributes attr)]))]
              [(or (time-tai-type? attr-type) (time-utc-type? attr-type))
               (make-default-binder attr (new snooze-date-field% [predicate (by-attributes attr)]))]
              [(and (string-type? attr-type) (or (not (character-type-max-length attr-type))
                                                 (> (character-type-max-length attr-type) 128)))
               (make-default-binder attr (new snooze-text-area% [predicate (by-attributes attr)]))]
              [else 
               (make-default-binder attr (new snooze-text-field% [predicate (by-attributes attr)]))])))
    
    ; attribute -> form-element<%>
    (define/public (get-binder attribute)
      (hash-ref attr=>binder attribute))
    
    ; seed attribute -> xml
    (define/override (render-attribute seed attr)
      (let ([editors (binder-editors (get-binder attr))])
        (xml (tr (th ,(render-attribute-label seed attr))
                 (td ,@(for/list ([editor (in-list editors)])
                         (send editor render seed)))))))
    
    ; seed -> xml
    (define/augment (render seed)
      (xml (table (@ [class 'crud-create-update])
                  (tbody ,(render-attributes seed (get-attributes))))
           ,(send submit-button render seed)))
    
    
    ; Validation ---------------------------------
    
    ; struct -> struct
    (define/public (update-struct struct)
      (apply snooze-struct-set 
             (list* struct
                    (flatten (for/fold ([attrs+vals null])
                               ([attr  (in-list (get-attributes))])
                               (append attrs+vals 
                                       ((binder-values (get-binder attr)))))))))
    
    ; -> (listof check-result)
    (define/overment (validate)
      (super set-struct! (update-struct (get-struct)))
      (inner (error "validate must be augmented") validate)) ; apply the validation procedure
    
    ; -> struct
    (define/override (commit-changes)
      (call-with-transaction
       (lambda () (let ([struct (save! (get-struct))])
                    (clear-continuation-table!)
                    struct))))))

; Provides ---------------------------------------
(provide (all-defined-out))