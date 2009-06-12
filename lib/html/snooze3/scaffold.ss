#lang scheme

(require (planet untyped/snooze:3)
         (planet untyped/unlib:3/debug)
         "../html-element.ss"
         "../html-page.ss"
         "scaffold/interfaces.ss"
         "scaffold/default-mixins.ss"
         "report.ss")

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

(define (scaffold-report-element entity
                                 [report-class% (default-snooze-report-crudl-mixin snooze-report%)]
                                 #:crudl-mixin        [crudl-mixin        (default-crudl-mixin entity)]
                                 #:rdl-mixin          [rdl-mixin          (default-review+delete+list-mixin)]
                                 #:list-mixin         [list-mixin         (default-list-mixin)]
                                 #:crudl-report-mixin [crudl-report-mixin (default-crudl-report-mixin)]
                                 #:report-mixin       [report-mixin       (default-report-mixin report-class%)])
  (lambda (element)
    (report-mixin (crudl-report-mixin (list-mixin (rdl-mixin (crudl-mixin element)))))))


; Provides ---------------------------------------

(provide (all-defined-out)
         (all-from-out "scaffold/default-mixins.ss")
         (all-from-out "scaffold/interfaces.ss"))

; DJG : Object/class contracts are in the works. Leave this kind of thing for now.

;(provide/contract
; [scaffold-review-element (->* (entity?)
;                               (#:crudl-mixin   crudl-mixin/c
;                                                #:crud-mixin   crud-mixin/c
;                                                #:rdl-mixin    review+delete+list-mixin/c
;                                                #:rd-mixin     review+delete-mixin/c
;                                                #:review-mixin review+delete-mixin/c)
;                               (-> (implementation?/c html-element<%>)
;                                   (implementation?/c crudl-review+delete<%>)))])
