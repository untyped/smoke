#lang scheme

(require (planet untyped/snooze:2)
         "../../../lib-base.ss"
         "../submit-button.ss"
         "editor.ss"
         "form-element.ss")

; Review pages -----------------------------------

; entity [(attr -> (value -> xml))] -> (struct -> xml)
(define (entity->review-renderer entity [attr->review-renderer default-attr->review-renderer])
  (let ([attributes (entity-attributes entity)])
    (lambda (struct)
      (xml (dl (@ [class 'snooze-review-entity])
               ,@(for/list ([attr (in-list attributes)])
                   ((attr->review-renderer attr) (struct-attribute struct attr))))))))

; attr -> (value -> xml)p
(define (default-attr->review-renderer attr)
  (lambda (value)
    (xml (dt (@ [class 'snooze-review-attr])  ,(attribute-name attr))
         (dd (@ [class 'snooze-review-value]) ,value))))

; List pages -------------------------------------

; struct [(attr -> (value -> xml))] -> ((listof struct) -> xml)
(define (entity->list-renderer entity [attr->list-renderer default-attr->list-renderer])
  (let* ([attributes (entity-attributes entity)])
    (lambda (structs)
      (xml (table (@ [class 'snooze-list])
                  (thead (tr ,@(for/list ([attr (in-list attributes)])
                                 (xml (th ,(attribute-name attr))))))
                  (tbody ,@(for/list ([struct (in-list structs)])
                             (struct->list-xml struct attr->list-renderer))))))))

; snooze-struct (attr -> (value -> xml)) -> xml
(define (struct->list-xml struct attr->renderer)
  (let* ([entity     (struct-entity struct)]
         [attributes (entity-attributes entity)])
    (xml (tr ,@(for/list ([attr (in-list attributes)])
                 ((attr->renderer attr) (struct-attribute struct attr)))))))

; attr -> (value -> xml)
(define (default-attr->list-renderer attr)
  (lambda (value)
    (xml (td ,value))))


; Editors ----------------------------------------

; snooze-struct (attr -> form-element) -> form-element%
(define (entity->editor% entity [editor% snooze-editor%] [attr->editor default-attr->editor])
  (let ([attributes (entity-attributes entity)])
    (class/cells editor% ()
      
      ; (cell (U snooze-struct #f))
      (init-cell struct #f #:accessor)
      
      ; (listof form-element%)
      (field fields
             (for/list ([attr (in-list attributes)])
               (attr->editor attr))
             #:accessor
             #:children)
      
      (field submit-button
             (new submit-button%
                  [action (callback on-update)]
                  [label  "Okay"])
             #:accessor
             #:child)
      
      ; snooze-struct -> void
      (define/public (set-struct! struct)
        (web-cell-set! struct-cell struct)
        (for ([val   (in-list (struct-attributes struct))]
              [field (in-list fields)])
          (send field set-value! val)))
      
      ; seed -> xml
      (define/augment (render seed)
        (xml (table (tbody ,@(for/list ([field (in-list fields)]
                                        [attr  (in-list attributes)])
                               (xml (tr (th ,(attribute-name attr))
                                        (td ,(send field render seed)))))))
             ,(send submit-button render seed)))
      
      ; -> any
      (define/public #:callback/return (on-update)
        (error "on-update must be overridden")))))

; attr -> html-element
(define (default-attr->editor attr)
  (new snooze-text-field% [predicate (by-attributes attr)]))

; Provides ---------------------------------------

(provide (all-defined-out))