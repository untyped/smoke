#lang scheme/base

(require (only-in srfi/1 delete)
         "../../lib-base.ss"
         "form-element.ss"
         "html-element.ss"
         "text-field.ss")

(define multi-select%
  (class/cells html-element% (form-element<%>)
    
    (inherit core-html-attributes)
    
    ; Fields -------------------------------------
    
    ; (cell (listof any))
    (init-cell items null #:accessor #:mutator)
    
    ; form-element<%>
    (init-field editor
      (new text-field%)
      #:child #:accessor)
    
    ; (cell boolean)
    (init-cell editor-visible? #f #:accessor #:mutator)
    
    ; Constructor --------------------------------
    
    ; (listof (U symbol string))
    (init [classes null])
    
    (super-new [classes (cons 'ui-widget classes)])
    
    ; Methods ------------------------------------
    
    ; -> boolean
    (define/public (get-enabled?)
      #t)
    
    ; boolean -> void
    (define/public (set-enabled?! enabled)
      (error "not implemented yet"))
    
    ; -> list
    (define/public (get-value)
      (get-items))
    
    ; list -> void
    (define/public (set-value! value)
      (set-items! value))
    
    ; -> boolean
    (define/public (value-valid?)
      #t)
    
    ; -> (U string #f)
    (define/public (get-value-error)
      #f)
    
    ; -> boolean
    (define/public (value-changed?)
      (web-cell-changed? items-cell))
    
    ; any -> void
    (define (add-item! item)
      (set-items! (append (get-items) (list item))))
    
    ; any -> void
    (define (delete-item! item)
      (set-items! (delete item (get-items))))
    
    ; seed any -> xml
    (define (render-item seed item)
      (xml ,item))
    
    ; seed -> xml
    (define/override (render seed)
      (xml (div (@ ,(core-html-attributes seed))
                (ul (@ [class "items"])
                    ,@(for/list ([item (get-items)] [index (in-naturals)])
                        (xml (li ,(render-item seed item)
                                 " " (a (@ [onclick ,(embed/ajax seed (callback on-item-delete index))]) "Delete")))))
                ,(if (get-editor-visible?)
                     (xml (div (@ [class "controls editor"])
                               ,(send editor render seed)
                               " " (a (@ [onclick ,(embed/ajax seed (callback on-editor-confirm))]) "Add")
                               " " (a (@ [onclick ,(embed/ajax seed (callback on-editor-cancel))]) "Cancel")))
                     (xml (div (@ [class "controls no-editor"])
                               (a (@ [onclick ,(embed/ajax seed (callback on-editor-show))])
                                  "Add item...")))))))
    
    ; -> void
    (define/public #:callback (on-editor-show)
      (set-editor-visible?! #t))
    
    ; -> void
    (define/public #:callback (on-editor-confirm)
      (with-handlers ([exn:smoke:form? void])
        (add-item! (send editor get-value))
        (set-editor-visible?! #f)))
    
    ; -> void
    (define/public #:callback (on-editor-cancel)
      (send editor set-value! #f)
      (set-editor-visible?! #f))
    
    ; integer -> void
    (define/public #:callback (on-item-delete delete-index)
      (delete-item! (list-ref (get-items) delete-index)))))

; Provide statements -----------------------------

(provide multi-select%)
