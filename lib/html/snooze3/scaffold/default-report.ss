#lang scheme/base

(require srfi/13
         srfi/19
         (only-in (planet untyped/unlib:3/list) assemble-list)
         (planet untyped/unlib:3/time)
         (planet untyped/unlib:3/debug)
         (planet untyped/snooze:3)
         "../../../../lib-base.ss"
         "../../html-element.ss"
         "../report.ss"
         "default-abstract.ss"
         "interfaces.ss")

; Constants --------------------------------------

; symbol ...
(define-values (column-id:review column-id:update column-id:delete)
  (values 'review-col 'update-col 'delete-col))

; Interfaces -------------------------------------

(define snooze-report-crudl<%>
  (interface ()
    render-review-td   ; seed string -> xml
    render-update-td   ; seed string -> xml
    render-delete-td)) ; seed string -> xml

; Mixins -----------------------------------------

(define (default-crudl-report-mixin)
  (mixin/cells (crudl-review+delete+list<%>) (crudl-report<%>)
    
    (inherit get-entity get-attributes)
    
    ; Methods ------------------------------------
    ; -> (sql-where 
    ;     [#:order  (listof sql-order)]
    ;     [#:offset (U integer #f)]
    ;     [#:limit  (U integer #f)]
    ;   ->
    ;    sql
    (define/public (make-query)
      (lambda (where-clause #:order  [order-clause null]
                            #:offset [offset       #f]
                            #:limit  [limit        #f])
        (let-alias ([E (get-entity)])
          (sql (select #:from   E
                       #:where  ,where-clause
                       #:order  ,order-clause
                       #:offset ,offset
                       #:limit  ,limit)))))
    
    ; -> (listof column)
    (define/public (make-columns)
      (make-columns/defaults (get-entity) (get-attributes) 
                             #:entity->url? (lambda (type entity) (entity->crudl-url? type entity))))
    
    ; -> (listof filter)
    (define/public (make-filters)
      (make-filters/attributes (get-attributes)))
    
    ; -> (listof view)
    (define/public (make-views)
      (default-views (make-columns)))
    
    ; crudl-operation entity -> boolean
    (define/public (entity->crudl-url? type entity)
      (error "entity->crudl-url? must be overridden"))
    
    ; seed solumn any -> xml
    (define/public (render-column seed column data)
      (error (format "Unrecognised column: ~a" column)))))

(define default-snooze-report-crudl-mixin
  (mixin/cells () (snooze-report-crudl<%>)
    
    ; seed string -> xml
    (define/public (render-review-td seed url)
      (xml (td (@ [class 'ui-icon-td]) 
               (a (@ [href ,url]) "Review"))))
    
    ; seed string -> xml
    (define/public (render-update-td seed url)
      (xml (td (@ [class 'ui-icon-td]) 
               (a (@ [href ,url]) "Update"))))
    
    ; seed string -> xml
    (define/public (render-delete-td seed url)
      (xml (td (@ [class 'ui-icon-td]) 
               (a (@ [href ,url]) "Delete"))))))



(define (default-report-mixin [report-class% (default-snooze-report-crudl-mixin snooze-report%)])
  (mixin/cells (crudl-report<%>) (crudl-report<%>)
    
    (inherit get-entity
             get-attributes
             get-attribute-pretty-name
             render-value
             entity->crudl-url?
             struct->crud-url
             make-columns
             make-views
             make-filters
             make-query
             render-column)
    
    ; Fields ----------------------------
    
    ; snooze-report%
    (field report
           (new (default-crudl-report (get-entity) report-class%
                                      #:attributes      (get-attributes)
                                      #:entity->url?    (lambda (type entity) (entity->crudl-url? type entity))
                                      #:struct->url     (lambda (type struct) (struct->crud-url type struct))
                                      #:report-columns  (make-columns)
                                      #:report-views    (make-views)
                                      #:report-filters  (make-filters)
                                      #:render-value    (lambda (seed attr value) (render-value seed attr value))
                                      #:query           (make-query)
                                      #:column-renderer (lambda (seed col data) (render-column seed col data)))) 
           #:child #:accessor)
    
    ; Methods ------------------------------------
    
    (define/augride (render seed)
      (send report render seed))))



(define (default-crudl-report entity 
                              [report-class% (default-snooze-report-crudl-mixin snooze-report%)]
                              #:attributes      [attributes     (default-attributes entity)]
                              #:entity->url?    [entity->url?   (lambda (crudl:op entity) #f)]
                              #:struct->url     [struct->url    (lambda (crudl:op struct) #f)]
                              #:report-columns  [report-columns (make-columns/defaults entity
                                                                                      attributes 
                                                                                      #:entity->url? entity->url?)]
                              #:report-views    [report-views   (default-views report-columns)]
                              #:report-filters  [report-filters (make-filters/attributes attributes)]
                              #:render-value    [render-value   (lambda (seed attr val)
                                                                 (xml ,(format "~a" val)))]
                              #:query           [query          (default-query entity)]
                              #:column-renderer [column-render default-column-renderer])
  (define-alias E entity)
  (class/cells report-class% (snooze-report-crudl<%>)
    
    (inherit get-sort-order render-review-td render-update-td render-delete-td)
    
    (super-new [sort-col (get-sort-column)])
    
    ; Get the default sort column. Defaults to the first non-CRUD column
    ; -> column
    (define/public (get-sort-column)
      (for/or ([col (in-list report-columns)])
        (and (not (memq (send col get-id) (list column-id:review column-id:update column-id:delete)))
             col)))
    
    (define/override (query-num-items filter pattern)
      ; E.guid is always present
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
      report-views)
    
    (define/override (get-filters)
      report-filters)
    
    ; seed symbol snooze-struct -> xml
    (define/public (render-item/crud seed col-id struct)
      (cond [(eq? col-id column-id:review)
             (render-review-td seed (struct->url crudl:review struct))]
            [(eq? col-id column-id:update)
             (render-update-td seed (struct->url crudl:update struct))]
            [(eq? col-id column-id:delete)
             (render-delete-td seed (struct->url crudl:delete struct))]
            [else (error "Unrecognised column")]))
    
    ; seed snooze-struct attribute -> xml
    (define/public (render-item/struct seed struct attribute)
      (xml (td ,(render-value seed attribute (snooze-struct-ref struct attribute)))))
    
    ; seed column any -> xml
    (define/public (render-item/custom seed col data)
      (column-render seed col data))
    
    ; seed (listof column) persistent-struct -> xml
    (define/override (render-item seed cols a-struct)
      (xml (tr ,@(for/list ([col (in-list cols)])
                   (xml ,(let ([col-id (send col get-id)])
                           (cond [(memq col-id (list column-id:review column-id:update column-id:delete))
                                  (render-item/crud seed col-id a-struct)]
                                 [(entity-has-attribute? entity col-id)
                                  (render-item/struct seed a-struct (entity-attribute entity col-id))]
                                 [else 
                                  (render-item/custom seed col a-struct)])))))))))


; entity -> (listof attribute)
(define (default-attributes entity)
  (cddr (entity-attributes entity)))

; struct -> (listof any)
(define (default-struct-ref* struct)
  (cddr (snooze-struct-ref* struct)))

; crudl-operation entity -> boolean
(define (default-entity->url? crudl-op entity)
  #f)

; crudl-operation struct -> (U string #f)
(define (default-struct->url crudl-op struct)
  #f)

;  [#:review-column? boolean]
;  [#:update-column? boolean]
;  [#:delete-column? boolean]
; -> 
;  (listof column)
(define (make-columns/crud #:review-column? [review-column? #f]
                           #:update-column? [update-column? #f]
                           #:delete-column? [delete-column? #f])
  (assemble-list [review-column? (make-column column-id:review "")]
                 [update-column? (make-column column-id:update "")]
                 [delete-column? (make-column column-id:delete "")]))

; entity attribute -> column
(define (make-column/attribute entity attr)
  (define-alias E entity)
  (let ([ATTR (sql:alias E attr)])
    (make-column (attribute-name attr)
                 (string-titlecase (attribute-pretty-name attr))
                 (list (sql:asc ATTR)))))

; entity (listof attribute) -> (listof column)
(define (make-columns/attributes entity attributes)
  (for/list ([attr (in-list attributes)])
    (make-column/attribute entity attr)))

; entity attributes #:entity->url? [(crudl:operation entity -> boolean)] -> (listof column)
(define (make-columns/defaults entity attributes #:entity->url? [entity->url? (lambda (type entity) #f)])
  (append (make-columns/crud #:review-column? (entity->url? crudl:review entity)
                             #:update-column? (entity->url? crudl:update entity)
                             #:delete-column? (entity->url? crudl:delete entity))
          (make-columns/attributes entity attributes)))

; attribute -> filter
(define (make-filter/attribute attr)
  (make-filter (attribute-name attr) ; filter IDs are eq? to attribute-names
               (format "~a" (attribute-pretty-name attr))))

; (listof attribute) -> (listof filter)
(define (make-filters/attributes attributes)
  (for/list ([attr (in-list attributes)])
    (make-filter/attribute attr)))

; (listof column) -> (listof view)
(define (default-views columns)
  (list (make-view 'default-view "Default" columns)))
  
; seed column any -> xml
(define (default-column-renderer seed column data)
      (error (format "Unrecognised column: ~a" column)))

(define (default-query entity)
  (lambda (where-clause #:order  [order-clause null]
                        #:offset [offset       #f]
                        #:limit  [limit        #f])
    (let-alias ([E entity])
      (sql (select #:from   E
                   #:where  ,where-clause
                   #:order  ,order-clause
                   #:offset ,offset
                   #:limit  ,limit)))))





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