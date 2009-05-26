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
    (field attr=>editor (make-hash) #:accessor)
    
    ; (listof form-element%)
    (field fields (init-fields) #:accessor #:children)
    
    ; submit-button%
    (field submit-button 
           (new submit-button% [action (callback on-update)] [label (get-button-label)]) 
           #:accessor #:child)
    
    ; Methods ------------------------------------
    
    ; (listof form-element<%>)
    (define/private (init-fields)
      (for/list ([attr (in-list (get-attributes))])
        (let ([editor (make-editor attr)])
          (hash-set! attr=>editor attr editor)
          editor)))
    
    
    ; snooze-struct -> void
    (define/override (set-struct! struct)
      (super set-struct! struct)
      (for ([attr (in-list (get-attributes))])
        (let ([val   (snooze-struct-ref struct attr)]
              [field (get-editor attr)])
          (send field set-value! 
                ; default types; otherwise convert to a string and show in a textfield
                (cond [(guid? val)     val]
                      [(boolean? val)  val]
                      [(integer? val)  val]
                      [(time-tai? val) (time-tai->date val)] ; TODO time/date fields
                      [(time-utc? val) (time-utc->date val)]
                      [else            (format "~a" val)])))))
    
    ; seed attribute any -> xml
    (define/public (make-editor attr)
      (let ([attr-type (attribute-type attr)])
        (cond [(guid-type? attr-type)
               (new snooze-foreign-key-combo-box%
                    [predicate (by-attributes attr)]
                    [entity    (guid-type-entity attr-type)])]
              [(boolean-type? attr-type)
               (new snooze-check-box% [predicate (by-attributes attr)])]
              [(integer-type? attr-type)
               (new snooze-integer-field% [predicate (by-attributes attr)])]
              [(or (time-tai-type? attr-type) (time-utc-type? attr-type))
               (new snooze-date-field% [predicate (by-attributes attr)])]
              [(and (string-type? attr-type) (or (not (character-type-max-length attr-type))
                                                 (> (character-type-max-length attr-type) 128)))
               (new snooze-text-area% [predicate (by-attributes attr)])]
              [else 
               (new snooze-text-field% [predicate (by-attributes attr)])])))
    
    ; attribute -> form-element<%>
    (define/public (get-editor attribute)
      (hash-ref attr=>editor attribute))
    
    ; seed attribute -> xml
    (define/override (render-attribute seed attr)
      (xml (tr (th ,(render-attribute-label seed attr))
               (td ,(send (get-editor attr) render seed)))))
 
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
                    (for/fold ([args null])
                      ([attr  (in-list (get-attributes))]
                       [field (in-list fields)])
                      (list* attr (send field get-value) args)))))
    
    ; -> (listof check-result)
    (define/overment (validate)
      (super set-struct! (update-struct (get-struct)))
      (inner (error "validate must be augmented") validate)) ; apply the validation procedure
    
    ; -> struct
    (define/override (commit-changes)
      (call-with-transaction
       (lambda () (save! (get-struct)))))))

; Provides ---------------------------------------
(provide (all-defined-out))