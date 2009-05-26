#lang scheme/base

(require srfi/13
         srfi/19
         (only-in (planet untyped/unlib:3/list) assemble-list)
         (planet untyped/unlib:3/time)
         (planet untyped/snooze:3)
         "../../../../lib-base.ss"
         "../../html-element.ss"
         "../report.ss"
         "default-abstract.ss"
         "scaffold-internal.ss")

; Mixins -----------------------------------------

(define (default-report-mixin entity)
  (mixin/cells (crudl-report<%>) (crudl-report<%>)
    
    (inherit get-attributes
             get-attribute-pretty-name
             render-value
             entity->crudl-url?
             struct->crud-url
             make-query)
    
    ; Fields ----------------------------
    
    ; snooze-report%
    (field report
           (new (default-crudl-report entity
                                      (lambda () (get-attributes))
                                      (lambda (attr) (get-attribute-pretty-name attr))
                                      (lambda (seed attr value) (render-value seed attr value))
                                      (lambda (type entity) (entity->crudl-url? type entity))
                                      (lambda (type struct) (struct->crud-url type struct))
                                      (lambda () (make-query)))) 
           #:child)
    
    ; Methods ------------------------------------
    
    ; seed -> xml
    (define/augment (render seed)
      (send report render seed))))

(define (default-crudl-report entity
                              get-attributes
                              get-attribute-pretty-name
                              render-value
                              entity->crudl-url?
                              struct->url
                              make-query)
  (define-alias E entity)
  (let* ([attributes     (get-attributes)]
         [report-columns (assemble-list
                          [(entity->crudl-url? crudl:review entity) (make-column 'view-col "")]
                          [(entity->crudl-url? crudl:update entity) (make-column 'edit-col "")]
                          [(entity->crudl-url? crudl:delete entity) (make-column 'delete-col "")]
                          [#t ,@(for/list ([attr (in-list attributes)])
                                  (let ([ATTR (sql:alias E attr)])
                                    (make-column (attribute-name attr)
                                                 (format "~a" (get-attribute-pretty-name attr))
                                                 (list (sql:asc ATTR)))))])]
         [report-filters (for/list ([attr (in-list attributes)])
                           (make-filter (attribute-name attr) ; filter IDs are eq? to attribute-names
                                        (format "~a" (get-attribute-pretty-name attr))))]
         [query          (make-query)])
    (class/cells snooze-report% ()
      
      (inherit get-sort-order)
      
      (super-new [sort-col (car report-columns)])
      
      (define/override (query-num-items filter pattern)
        (find-one (sql (select #:what (count E.guid) #:from ,(query (make-where filter pattern))))))
      
      (define/override (query-items filter pattern col dir start count)
        (g:find (query (make-where filter pattern) 
                       #:order (get-sort-order col dir)
                       #:offset start
                       #:limit count)))
      
      ; filter pattern
      (define/private (make-where filter pattern)
        (if pattern 
            (let ([sql-return 
                   (for/or ([attr (in-list attributes)])
                     (let ([ATTR (sql:alias E attr)])
                       (if (equal? (filter-id filter) (attribute-name attr))
                           (cond [(integer-type? attr)  (sql:= ATTR pattern)]
                                 [(boolean-type? attr)  (sql:= ATTR pattern)]
                                 [else (sql:regexp-match-ci ATTR (pattern->regexp pattern))])
                           #f)))])
              (or sql-return (sql #t)))
            (sql #t)))
      
      (define/override (get-views)
        (list (make-view 'default-view "Default" report-columns)))
      
      (define/override (get-filters)
        report-filters)
      
      ; seed (listof column) persistent-struct -> xml
      (define/override (render-item seed cols a-struct)
        (xml (tr ,@(for/list ([col (in-list cols)])
                     (xml (td ,(let ([col-id (send col get-id)])
                                 (cond [(eq? col-id 'view-col)
                                        (xml (a (@ [href ,(struct->url crudl:review a-struct)]) "View"))]
                                       [(eq? col-id 'edit-col)
                                        (xml (a (@ [href ,(struct->url crudl:update a-struct)]) "Edit"))]
                                       [(eq? col-id 'delete-col)
                                        (xml (a (@ [href ,(struct->url crudl:delete a-struct)]) "Delete"))]
                                       [else
                                        (let ([attribute (entity-attribute entity col-id)])
                                          (render-value seed attribute (snooze-struct-ref a-struct attribute)))])))))))))))

; string [boolean] -> string
(define (pattern->regexp pattern [anywhere? #f])
  (apply string-append (cons (if anywhere? "^.*" "^")
                             (string-fold-right (lambda (chr accum)
                                                  (cond [(eq? chr #\*) (cons ".*" accum)]
                                                        [(eq? chr #\?) (cons "." accum)]
                                                        [else          (cons (regexp-quote (string chr)) accum)]))
                                                null
                                                pattern))))

; Provides ---------------------------------------
(provide (all-defined-out))