#lang scheme

(require srfi/13
         srfi/19
         (only-in (planet untyped/unlib:3/list) assemble-list)
         (planet untyped/unlib:3/time)
         (planet untyped/snooze:3)
         "../../../lib-base.ss"
         "../submit-button.ss"
         "../html-element.ss"
         "editor.ss"
         "report.ss"
         "form-element.ss")




; Constants --------------------------------------

; symbol ...
(define crudl:create 'create)
(define crudl:review 'review)
(define crudl:update 'update)
(define crudl:delete 'delete)
(define crudl:list   'list)


; Interfaces -------------------------------------

(define scaffolded-crudl-element<%>
  (interface ()
    get-entity
    get-entity-attrs
    get-struct-values
    render-attr-name
    render-struct))

; All scaffolded elements must:
;  - render an entity (and in order to do so)
(define scaffolded-rdl-element<%>
  (interface (scaffolded-crudl-element<%>)
    struct->crud-url
    render-attr             ; FINAL! calls render-plain-attr or render-foreign-key-attr as appropriate
    render-plain-attr
    render-foreign-key-attr))

(define scaffolded-crud-element<%>
  (interface ()
    set-struct!
    get-struct))



(define scaffolded-review+delete-element<%>
  (interface (scaffolded-rdl-element<%> scaffolded-crud-element<%>)
    render-attr-name+value))

(define scaffolded-create+update-element<%>
  (interface (scaffolded-crudl-element<%> scaffolded-crud-element<%>)
    render-editor-component))

(define scaffolded-list-element<%>
  (interface (scaffolded-rdl-element<%>)
    set-structs!
    get-structs))



(define (make-scaffolded-crudl-mixin entity)
  (mixin/cells (html-element<%>) (scaffolded-crudl-element<%>)
    
    (define/public (get-entity)
      entity)
    
    (define/public (get-entity-attrs)
      (cddr (entity-attributes entity)))
    
    (define/public (get-struct-values struct)
      (cddr (snooze-struct-ref* struct)))
    
    ; seed attribute -> xml
    (define/public (render-attr-name seed attr)
      (xml ,(attribute-pretty-name attr)))
    
    (define/public (render-struct seed struct)
      (error "render-struct must be overridden"))))



(define (make-scaffolded-rdl-mixin)
  (mixin/cells (scaffolded-crudl-element<%>) (scaffolded-rdl-element<%>)
    
    (define/public (struct->crud-url crudl-operation struct)
      #f)
    
    (define/public-final (render-attr seed attr value)
      (let ([attr-type (attribute-type attr)])
        (if (and (guid-type? attr-type) value)
            (render-foreign-key-attr seed attr value)
            (render-plain-attr seed attr value))))
    
    (define/public (render-plain-attr seed attr plain)
      (xml ,(format "~s" plain)))
    
    (define/public (render-foreign-key-attr seed attr struct)
      (let ([url           (struct->crud-url crudl:review struct)]
            [struct-pretty (format-snooze-struct struct)])
        (if url 
            (xml (a (@ [href ,url]) ,struct-pretty))
            (xml ,struct-pretty))))))


(define (make-scaffolded-review+delete-mixin)
  (mixin/cells (scaffolded-crudl-element<%> scaffolded-rdl-element<%>) (scaffolded-review+delete-element<%>)
    
    (inherit get-entity-attrs
             render-attr-name
             render-attr)
    
    (define/public (render-attr-name+value seed attr value)
      (xml (tr (th ,(render-attr-name seed attr))
               (td ,(render-attr seed attr value)))))
    
    (define/override (render-struct seed struct)
      (xml (table (@ [class 'crud-review-delete])
                  (tbody ,@(for/list ([attr (in-list (get-entity-attrs))])
                             (render-attr-name+value seed attr (snooze-struct-ref struct attr)))))))))

(define (make-scaffolded-review-mixin)
  (mixin/cells (html-element<%> scaffolded-crud-element<%> scaffolded-crudl-element<%> scaffolded-rdl-element<%>) 
    (scaffolded-review+delete-element<%>)
    
    (inherit get-struct render-struct)
    
    (define/augment (render seed)
      (render-struct seed (get-struct)))))



(define (make-crud-mixin)
  (mixin/cells (html-element<%>) (scaffolded-crud-element<%>)
    ; Fields -----------------------------------
    ; (cell (U snooze-struct #f))
    (init-cell struct #f #:accessor #:mutator)))


(define (scaffold-review-page entity
                              #:crudl-mixin [crudl-mixin (make-scaffolded-crudl-mixin entity)]
                              #:crud-mixin  [crud-mixin  (make-crud-mixin)]
                              #:rdl-mixin   [rdl-mixin   (make-scaffolded-rdl-mixin)]
                              #:rd-mixin    [rd-mixin    (make-scaffolded-review+delete-mixin)]
                              #:r-mixin     [r-mixin     (make-scaffolded-review-mixin)])
  (lambda (element)
    (r-mixin (rd-mixin (rdl-mixin (crud-mixin (crudl-mixin element)))))))


(provide (except-out (all-defined-out)
                     crudl:create
                     crudl:review
                     crudl:update
                     crudl:delete
                     crudl:list))