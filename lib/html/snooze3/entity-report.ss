#lang scheme/base

(require "../../../lib-base.ss")

(require (only-in srfi/13 string-fold-right)
         (planet untyped/dispatch:3)
         (planet untyped/snooze:3)
         (planet untyped/unlib:3/for)
         (planet untyped/unlib:3/string)
         (planet untyped/unlib:3/symbol)
         "../jquery-ui-util.ss"
         "controller-internal.ss"
         "report-column.ss"
         "report-internal.ss")

; Columns ----------------------------------------

(define attribute-report-column%
  (class/cells snooze-report-column% ()
    
    ; (listof attribute)
    (init-field attribute #:accessor)
    
    (init [id          (string->symbol
                        (format "~a-~a"
                                (entity-name (attribute-entity attribute))
                                (attribute-name attribute)))]
          [string-name (string-sentencecase (attribute-pretty-name attribute))]
          [order       (list (sql:order (sql:alias (entity-default-alias (attribute-entity attribute))
                                                   attribute)
                                        'asc))])
    
    (super-new [id          id]
               [string-name string-name]
               [order       order])))

; attribute -> column
(define (default-attribute-column attr)
  ((attribute-column-defaults) attr))

; (parameter (attribute -> column))
(define attribute-column-defaults
  (make-parameter (lambda (attr) (new attribute-report-column% [attribute attr]))))

; Classes ----------------------------------------

(define entity-report%
  (class/cells snooze-report% ()
    
    (inherit get-sort-col
             get-sort-dir
             get-sort-order
             get-visible-columns) 
    
    ; Fields -------------------------------------
    
    ; Constructor --------------------------------
    
    ; entity
    (init-field entity #:accessor)
    
    ; (listof attribute)
    (init [attributes (and entity (entity-data-attributes entity))])
    
    ; (cell boolean)
    (init-cell review-column? #t #:accessor show-review-column? #:mutator set-show-review-column?!)
    (init-cell update-column? #t #:accessor show-update-column? #:mutator set-show-update-column?!)
    (init-cell delete-column? #t #:accessor show-delete-column? #:mutator set-show-delete-column?!)
    
    ; (listof editor<%>)
    (init-field columns
      (or (and attributes (map default-attribute-column attributes))
          (error "entity-report constructor: insufficient arguments"))
      #:accessor)
    
    ; (cell (listof view)
    (init-field views 
      (list (make-view 'default "Default" columns))
      #:override-accessor)
    
    (init-field filters null #:override-accessor)
    
    (init [sort-col (car columns)])
    (init [classes  null])
    
    (super-new [sort-col sort-col]
               [classes  (list* 'smoke-entity-report 'ui-widget classes)])
    
    ; Methods ------------------------------------
    
    ; filter string -> natural
    (define/override (query-num-items filter pattern)
      (let-sql ([entity (get-entity)])
        (find-one (sql (select #:what  (count entity.guid)
                               #:from  entity
                               #:where ,(make-where filter pattern))))))
    
    ; filter string column (U 'asc 'desc) natural natural -> (gen-> result)
    (define/override (query-items filter pattern col dir start count)
      (let-sql ([entity (get-entity)])
        (g:find (sql (select #:from   entity
                             #:where  ,(make-where filter pattern) 
                             #:order  ,(get-sort-order col dir)
                             #:offset ,start
                             #:limit  ,count)))))
    
    ; filter pattern
    (define/public (make-where filter pattern)
      (let ([entity (get-entity)])
        (if pattern 
            (apply sql:or
                   (for/fold/reverse
                    ([accum null])
                    ([col   (in-list (get-visible-columns))])
                    (if (is-a? col attribute-report-column%)
                        (let* ([attr (send col get-attribute)]
                               [type (attribute-type attr)]
                               [ATTR (sql:alias (entity-default-alias entity) attr)])
                          (cond [(boolean-type? type)   (cons (sql:= ATTR pattern) accum)]
                                [(numeric-type? type)   (cons (sql:= ATTR pattern) accum)]
                                [(character-type? type) (cons (sql:regexp-match-ci ATTR (pattern->regexp pattern)) accum)]
                                [else                   accum]))
                        accum)))
            (sql #t))))
    
    ; seed (listof column) -> xml
    (define/override (render-empty-body seed cols)
      (xml (tbody (tr (td (@ [colspan ,(+ (if (and (show-review-column?) (review-controller-set? (get-entity))) 1 0)
                                          (if (and (show-update-column?) (update-controller-set? (get-entity))) 1 0)
                                          (if (and (show-delete-column?) (delete-controller-set? (get-entity))) 1 0)
                                          (length cols))]
                             [class "empty-row"])
                          "There are no items to display in this list.")))))
    
    ; seed (listof column) -> xml
    (define/override (render-head seed cols)
      ; column
      (define current-col (get-sort-col))
      ; (U 'asc 'desc)
      (define current-dir (get-sort-dir))
      ; xml
      (xml (thead (tr (@ [class 'ui-widget-header])
                      ,(render-controllers-th seed)
                      ,@(for/list ([col (in-list (get-visible-columns))])
                          (send col render-head seed (and (equal? col current-col) current-dir)))))))
    
    ; seed (listof column) snooze-struct -> xml
    (define/override (render-item seed cols struct)
      (xml (tr ,(render-controllers-td seed struct)
               ,@(for/list ([col (in-list cols)])
                   (render-column seed col struct)))))
    
    ; seed column snooze-struct -> xml
    (define/public (render-column seed col struct)
      (if (is-a? col attribute-report-column%)
          (let ([attr (send col get-attribute)])
            (render-value-td seed attr (snooze-struct-ref struct attr)))
          (error "entity-report.render-column: could not render column" col)))
    
    ; seed -> xml
    (define/public (render-controllers-th seed)
      (opt-xml (or (and (show-review-column?) (review-controller-set? (get-entity)))
                   (and (show-update-column?) (update-controller-set? (get-entity)))
                   (and (show-delete-column?) (delete-controller-set? (get-entity))))
        (th)))
    
    ; seed string -> xml
    (define/public (render-controllers-td seed struct)
      (opt-xml (or (and (show-review-column?) (review-controller-set? struct))
                   (and (show-update-column?) (update-controller-set? (get-entity)))
                   (and (show-delete-column?) (delete-controller-set? (get-entity))))
        (td ,(controller-link (review-controller-ref struct) struct
                              #:body (xml (div (@ [class "controller-icon ui-state-default ui-corner-all"]
                                                  [title "View this item"])
                                               (!icon (@ [type "search"]))))
                              #:else (xml (div (@ [class "controller-icon ui-state-disabled ui-corner-all"]
                                                  [title "Cannot view this item"])
                                               (!icon (@ [type "search"])))))
            ,(controller-link (update-controller-ref struct) struct
                              #:body (xml (div (@ [class "controller-icon ui-state-default ui-corner-all"]
                                                  [title "Edit this item"])
                                               (!icon (@ [type "pencil"]))))
                              #:else (xml (div (@ [class "controller-icon ui-state-disabled ui-corner-all"]
                                                  [title "Cannot edit this item"])
                                               (!icon (@ [type "pencil"])))))
            ,(controller-link (delete-controller-ref struct) struct
                              #:body (xml (div (@ [class "controller-icon ui-state-default ui-corner-all"]
                                                  [title "Delete this item"])
                                               (!icon (@ [type "trash"]))))
                              #:else (xml (div (@ [class "controller-icon ui-state-disabled ui-corner-all"]
                                                  [title "Cannot delete this item"])
                                               (!icon (@ [type "trash"]))))))))
    
    ; seed attribute any -> xml
    (define/public (render-value-td seed attr val)
      (xml (td ,(if (snooze-struct? val)
                    (if (review-controller-set? val)
                        (xml (a (@ [href ,(review-controller-url val)])
                                ,(format-snooze-struct val)))
                        (xml-quote (format-snooze-struct val)))
                    (xml-quote val)))))
    
    ; string [boolean] -> string
    (define/public (pattern->regexp pattern [anywhere? #f])
      (apply string-append
             (cons (if anywhere? "^.*" "^")
                   (string-fold-right (lambda (chr accum)
                                        (cond [(eq? chr #\*) (cons ".*" accum)]
                                              [(eq? chr #\?) (cons "." accum)]
                                              [else          (cons (regexp-quote (string chr)) accum)]))
                                      null
                                      pattern))))))

; Provide statements -----------------------------

(provide entity-report%
         attribute-report-column%)

(provide/contract
 [default-attribute-column  (-> attribute? (is-a?/c attribute-report-column%))]
 [attribute-column-defaults (parameter/c (-> attribute? (is-a?/c attribute-report-column%)))])
