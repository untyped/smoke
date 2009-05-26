#lang scheme

(require "scaffold-internal.ss"
         "scaffold-defaults.ss")

; Procedures -------------------------------------

; entity -> (mixinof html-element<%> -> crudl-review+delete<%>)
(define (scaffold-review-element entity
                                 #:crudl-mixin  [crudl-mixin  (default-crudl-mixin entity)]
                                 #:crud-mixin   [crud-mixin   (default-crud-mixin)]
                                 #:rdl-mixin    [rdl-mixin    (default-review+delete+list-mixin)]
                                 #:rd-mixin     [rd-mixin     (default-review+delete-mixin)]
                                 #:review-mixin [review-mixin (default-review-mixin)])
  (lambda (element)
    (review-mixin (rd-mixin (rdl-mixin (crud-mixin (crudl-mixin element)))))))

; entity -> (mixinof html-element<%> -> crudl-review+delete<%>)
(define (scaffold-update-element entity
                                 #:crudl-mixin  [crudl-mixin  (default-crudl-mixin entity)]
                                 #:crud-mixin   [crud-mixin   (default-crud-mixin)]
                                 #:editor-mixin [editor-mixin (default-crudl-editor-mixin)]
                                 #:rdl-mixin    [rdl-mixin    (default-review+delete+list-mixin)]
                                 #:rd-mixin     [rd-mixin     (default-review+delete-mixin)]
                                 #:update-mixin [update-mixin (default-create+update-mixin)])
  (lambda (element)
    (update-mixin (editor-mixin (rd-mixin (rdl-mixin (crud-mixin (crudl-mixin element))))))))

; entity -> (mixinof html-element<%> -> crudl-review+delete<%>)
(define (scaffold-delete-element entity
                                 #:crudl-mixin  [crudl-mixin  (default-crudl-mixin entity)]
                                 #:crud-mixin   [crud-mixin   (default-crud-mixin)]
                                 #:editor-mixin [editor-mixin (default-crudl-editor-mixin)]
                                 #:rdl-mixin    [rdl-mixin    (default-review+delete+list-mixin)]
                                 #:rd-mixin     [rd-mixin     (default-review+delete-mixin)]
                                 #:delete-mixin [delete-mixin (default-delete-mixin)])
  (lambda (element)
    (delete-mixin (editor-mixin (rd-mixin (rdl-mixin (crud-mixin (crudl-mixin element))))))))

; entity -> (mixinof html-element<%> -> crudl-review+delete<%>)
(define (scaffold-list-element entity
                               #:crudl-mixin      [crudl-mixin      (default-crudl-mixin entity)]
                               #:crudl-list-mixin [crudl-list-mixin (default-crudl-list-mixin entity)]
                               #:rdl-mixin        [rdl-mixin        (default-review+delete+list-mixin)]
                               #:list-mixin       [list-mixin       (default-list-mixin)])
  (lambda (element)
    (list-mixin (rd-mixin (rdl-mixin (crud-mixin (crudl-mixin element)))))))

; entity -> (mixinof html-element<%> -> crudl-review+delete<%>)
(define (scaffold-report-element entity
                                 #:crudl-mixin      [crudl-mixin      (default-crudl-mixin entity)]
                                 #:crudl-list-mixin [crudl-list-mixin (default-crudl-list-mixin entity)]
                                 #:rdl-mixin        [rdl-mixin        (default-review+delete+list-mixin)]
                                 #:report-mixin     [report-mixin     (default-report-mixin)])
  (lambda (element)
    (report-mixin (rd-mixin (rdl-mixin (crudl-list-mixin (crudl-mixin element)))))))

; Provides ---------------------------------------

(provide (all-defined-out)
         (all-from-out "scaffold-defaults.ss")
         (except-out (all-from-out "scaffold-internal.ss")
                     snooze-foreign-key-combo-box%
                     crudl:create
                     crudl:review
                     crudl:update
                     crudl:delete
                     crudl:list))