#lang scheme

(require "scaffold/scaffold-internal.ss"
         "scaffold/scaffold-defaults.ss")

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
                               #:rdl-mixin        [rdl-mixin        (default-review+delete+list-mixin)]
                               #:list-mixin       [list-mixin       (default-list-mixin)])
  (lambda (element)
    (list-mixin (rdl-mixin (crudl-mixin element)))))

; entity -> (mixinof html-element<%> -> crudl-review+delete<%>)
(define (scaffold-report-element entity
                                 #:crudl-mixin        [crudl-mixin        (default-crudl-mixin entity)]
                                 #:rdl-mixin          [rdl-mixin          (default-review+delete+list-mixin)]
                                 #:crudl-report-mixin [crudl-report-mixin (default-crudl-report-mixin)]
                                 #:report-mixin       [report-mixin       (default-report-mixin entity)])
  (lambda (element)
    (report-mixin (crudl-report-mixin (rdl-mixin (crudl-mixin element))))))

; Provides ---------------------------------------

(provide (all-defined-out)
         (all-from-out "scaffold/scaffold-defaults.ss")
         (all-from-out "scaffold/scaffold-internal.ss"))