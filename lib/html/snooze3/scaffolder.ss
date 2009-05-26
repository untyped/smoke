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

; Contracts --------------------------------------

(define entity->url/c
  (->* ((or/c 'create 'review 'update 'delete 'list)
        entity?)
       (snooze-struct?) 
       (or/c string? #f)))

(define struct->url/c
  (-> (or/c 'create 'review 'update 'delete 'list) 
      snooze-struct? 
      (or/c string? #f)))

; Procedures -------------------------------------

; entity -> (listof attribute)
(define (default-attributes entity)
  (cddr (entity-attributes entity))) ; trim the GUID and revision

; attribute -> string
(define default-attr-pretty-name 
  attribute-pretty-name)

; persistent-struct -> (listof any)
(define (default-struct-ref* struct)
  (cddr (snooze-struct-ref* struct))) ; trim the GUID and revision

; attribute any -> xml
(define (attr->xml attr 
                   value 
                   [plain-attr->xml       default-plain-attr->xml] 
                   [foreign-key-attr->xml default-foreign-key-attr->xml]
                   #:struct->url [struct->url default-struct->url])
  (let ([attr-type (attribute-type attr)])
    (if (and (guid-type? attr-type) value)
        (foreign-key-attr->xml attr value #:struct->url struct->url)
        (plain-attr->xml attr value))))

; attribute (not snooze-struct) -> xml
(define (default-plain-attr->xml attr value)
  (xml ,(format "~s" value)))

; attribute snooze-struct [#:struct->url struct->url/c] -> xml
(define (default-foreign-key-attr->xml attr value #:struct->url [struct->url default-struct->url])
  (let ([url (struct->url crudl:review value)])
    (opt-xml url (a (@ [href ,url]) ,(format-snooze-struct value)))))

; (U 'create 'review 'update 'delete 'list) [(U struct #f) -> (U url #f)
(define (default-entity->url crudl-type entity [struct #f])
  #f)

; (U 'create 'review 'update 'delete 'list) struct -> (U url #f)
(define (default-struct->url crudl-type struct)
  (default-entity->url crudl-type (snooze-struct-entity struct) struct))

; -> sql
(define (default-report-query entity)
  (lambda (where-clause [order-clause null] 
                        [offset #f]
                        [limit #f])
    ;(define-snooze-interface snooze)
    (let-alias ([E entity])
      (sql (select #:from   E 
                   #:where  ,where-clause
                   #:order  ,order-clause
                   #:offset ,offset
                   #:limit  ,limit)))))

; Interfaces -------------------------------------

; A really simple html-element creation mixin:
; mixin against any html-element and get to specify the XML as an init argument.
(define vanilla-element<%>
  (interface ()
    get-inner-xml    ; -> (listof snooze-form-element%)
    set-inner-xml!)) ; persistent-struct -> void

; The basis for scaffolding - pages must have a struct
(define snooze-scaffolded-element<%>
  (interface ()
    set-struct!  ; persistent-struct -> void 
    get-struct)) ; -> persistent-struct

; Editors add a list of fields, and must also subtype snooze-editor<%>
(define snooze-scaffolded-editor<%>
  (interface (snooze-scaffolded-element<%> snooze-editor<%>)
    get-fields))   ; -> (listof snooze-form-element%)

; The basis for list scaffolding - requires a list of structs
(define snooze-scaffolded-list-element<%>
  (interface ()
    set-structs!  ; (listof persistent-struct) -> void 
    get-structs)) ; -> (listof persistent-struct)

; Mixins -----------------------------------------
(define vanilla-element-mixin
  (mixin/cells (html-element<%>) (vanilla-element<%> html-element<%>)             
    ; xml
    (init-cell inner-xml #f #:accessor #:mutator)
    ; seed -> xml
    (define/augment (render seed)
      (get-inner-xml))))

; Review pages -----------------------------------

; A mixin for a particular entity review page.
;
;  entity
;  [(attr -> (value -> xml))]
;  [#:attributes       (listof attribute)]
;  [#:attr-pretty-name (attribute -> string)]
;  [#:struct->url      struct->url/c]
; -> 
;  (mixin html-element<%>)
(define (entity->review-mixin entity 
                              [attr->review-renderer default-attr->review-renderer]
                              #:attributes       [attributes       (default-attributes entity)]
                              #:attr-pretty-name [attr-pretty-name default-attr-pretty-name]
                              #:struct->url      [struct->url      default-struct->url])
  (let ([renderer (entity->review-renderer entity attr->review-renderer
                                           #:attributes       attributes
                                           #:attr-pretty-name attr-pretty-name
                                           #:struct->url      struct->url)])
    (mixin/cells (html-element<%>) (snooze-scaffolded-element<%>)
      
      ; Fields -----------------------------------
      ; (cell (U snooze-struct #f))
      (init-cell struct #f #:accessor #:mutator)
      
      ; Methods ------------------------------------ 
      ; seed -> xml
      (define/augment (render seed)
        (renderer (get-struct))))))

; Generates a procedure that takes persistent-structs for a particular entity
; and returns an appropriate XML representation.
; Uses the default attribute-renderer, but that may be replaced.
;
; entity [(attr -> (value -> xml))] -> 
;  entity
;  [(attr -> (value -> xml))]
;  [#:attributes  (listof attribute)]
; -> 
;  (struct -> xml)
(define (entity->review-renderer entity 
                                 [attr->review-renderer default-attr->review-renderer]
                                 #:attr-pretty-name       [attr-pretty-name       default-attr-pretty-name] 
                                 #:attributes             [attributes             (default-attributes entity)]
                                 #:struct->url            [struct->url            default-struct->url]
                                 #:plain-attr->xml        [plain-attr->xml        default-plain-attr->xml]
                                 #:foreign-key-attr->xml  [foreign-key-attr->xml  default-foreign-key-attr->xml])
  (lambda (struct)
    (xml (table (tbody (@ [class 'snooze-review-entity])
                       ,@(for/list ([attr (in-list attributes)])
                           ((attr->review-renderer attr attr-pretty-name plain-attr->xml foreign-key-attr->xml) 
                            (snooze-struct-ref struct attr))))))))

; The default attribute-value renderer.
;
;  attr 
;  [(attribute -> string)]
;  [(attribute -> xml)]
;  [(attribute -> xml)]
; ->
;  (value -> xml)
(define (default-attr->review-renderer attr 
          [attr-pretty-name      default-attr-pretty-name]
          [plain-attr->xml       default-plain-attr->xml]
          [foreign-key-attr->xml default-foreign-key-attr->xml]
          #:struct->url [struct->url default-struct->url])
  (let ([attr-type (attribute-type attr)])
    (lambda (value)
      (xml (tr (th (@ [class 'snooze-review-attr])
                   ,(attr-pretty-name attr))
               (td (@ [class 'snooze-review-value])
                   ,(attr->xml attr value plain-attr->xml foreign-key-attr->xml #:struct->url struct->url)))))))


; Editors ----------------------------------------

; Generates a mixin that acts as an editor element for persistent-structs of a given entity type.
;
; By default, all attributes except persistent-struct-id and persistent-struct-revision are editable.
; This behaviour may be changed by providing the list of attributes required using #:attributes.
; Similarly, the #:struct-ref* procedure retrieves the list of values for a given struct. This defaults
; to all values except id and revision.
;
; Editors are inferred using the attribute->editor procedure, which defaults to default-attr->editor.
; This behaviour may be changed by specifying a new attribute->editor procedure using #:attr->editor.
;
; Prior to saving, the persistent-struct is validated using the #:check-proc procedure.
; By default, no validation is performed; #:check-proc should be replaced with a genuine validation procedure.
;
;  snooze
;  entity
;  [#:attributes   (listof attribute)]
;  [#:struct-ref*  (persistent-struct -> (listof any))]
;  [#:attr->editor (attr -> snooze-form-element<%>)]
;  [#:check-proc   (persistent-struct -> (listof check-result))]
; ->
;  snooze-editor-mixin
(define (entity->editor-mixin snooze entity
                              #:attributes       [attributes       (default-attributes entity)]
                              #:attr-pretty-name [attr-pretty-name default-attr-pretty-name]
                              #:struct-ref*      [struct-ref*      default-struct-ref*]
                              #:attr->editor     [attr->editor     default-attr->editor] 
                              #:check-proc       [check-proc       (lambda (struct) null)])
  ; (snooze-editor% -> snooze-scaffolded-editor<%>)
  (define scaffolded-mixin
    (mixin/cells (snooze-editor<%> html-element<%>) (snooze-scaffolded-editor<%>)
      
      ; Fields -----------------------------------
      ; (cell (U snooze-struct #f))
      (init-cell struct #f #:accessor)
      
      ; (listof form-element%)
      (field fields (for/list ([attr (in-list attributes)]) (attr->editor attr)) #:accessor #:children)
      
      ; submit-button%
      (field submit-button (new submit-button% [action (callback on-update)] [label  "Okay"]) #:accessor #:child)
      
      ; Methods ----------------------------------
      
      ; snooze-struct -> void
      (define/public (set-struct! struct)
        (web-cell-set! struct-cell struct)
        (for ([val   (in-list (struct-ref* struct))]
              [field (in-list fields)])
          (send field set-value! 
                ; default types; otherwise convert to a string and show in a textfield
                (cond [(guid? val)     val]
                      [(boolean? val)  val]
                      [(integer? val)  val]
                      [(time-tai? val) (time-tai->date val)] ; TODO time/date fields
                      [(time-utc? val) (time-utc->date val)]
                      [else            (format "~a" val)]))))
      
      ; seed -> xml
      (define/augment (render seed)
        (xml (table (tbody ,@(for/list ([field (in-list fields)]
                                        [attr  (in-list attributes)])
                               (xml (tr (th ,(attr-pretty-name attr))
                                        (td ,(send field render seed)))))))
             ,(send submit-button render seed)))
      
      ; -> (listof check-result)
      (define/override (validate)
        (web-cell-set! 
         struct-cell 
         (apply snooze-struct-set 
                (list* (get-struct)
                       (for/fold ([args null])
                         ([attr  (in-list attributes)]
                          [field (in-list fields)])
                         (list* attr (send field get-value) args)))))
        (check-proc (get-struct))) ; apply the validation procedure
      
      ; -> struct
      (define/override (commit-changes)
        (let* ([struct (get-struct)]
               [entity (snooze-struct-entity struct)])
          (call-with-transaction
           ;(if (snooze-struct-saved? struct)
           ;    (format "Edit ~a details: ~a" (entity-name entity) struct)
           ;    (format "Create ~a instance: ~a" (entity-name entity) struct))
           (lambda () (save! struct)))))))
  ; Create a compound mixin of the above and snooze-editor-mixin
  (lambda (html-element) 
    (scaffolded-mixin (snooze-editor-mixin html-element))))

; The default attribute->editor procedure
;
; attr -> snooze-form-element<%>
(define (default-attr->editor attr)
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

; snooze-vanilla-combo-box%
(define snooze-foreign-key-combo-box%
  (class/cells snooze-vanilla-combo-box% ()
    
    (init-field entity #f #:accessor)
    
    ; -> (listof (cons integer string))
    (define/override (get-options)
      (let-alias ([E entity])
        (list* #f (select-all #:from E #:order ((asc E.guid))))))
    
    (define/override (option->raw option)
      (and option (guid? option) (snooze-struct-id option)))
    
    (define/override (raw->option raw)
      (and raw 
           (let ([id (string->number raw)])
             (and id (find-by-id entity id)))))
    
    (define/override (option->string option)
      (if option
          (format-snooze-struct option)
          (format "-- No ~a selected --" (entity-pretty-name entity))))))

; List pages -------------------------------------

; A mixin for a particular entity review page.
;
;  entity
;  [(attr -> (value -> xml))]
;  [#:attributes  (listof attribute)]
;  [#:struct-ref* (persistent-struct -> (listof any))]
; -> 
;  (mixin html-element<%>)
(define (entity->list-mixin entity 
                            [attr->review-renderer default-attr->list-renderer]
                            #:attributes  [attributes  (default-attributes entity)]
                            #:struct->url [struct->url default-struct->url])
  (let ([renderer (entity->list-renderer entity attr->review-renderer 
                                         #:attributes attributes
                                         #:struct->url struct->url)])
    (mixin/cells (html-element<%>) (snooze-scaffolded-list-element<%>)
      
      ; Fields -----------------------------------
      ; (cell (listof snooze-struct))
      (init-cell structs null #:accessor #:mutator)
      
      ; Methods ------------------------------------ 
      ; seed -> xml
      (define/augment (render seed)
        (renderer (get-structs))))))

;  entity
;  [(attr -> (value -> xml))]
;  [#:entity->attrs (entity -> (listof attribute))]
; ->
;  ((listof struct) -> xml)
(define (entity->list-renderer entity 
                               [attr->list-renderer default-attr->list-renderer]
                               #:attributes         [attributes       (default-attributes entity)]
                               #:attr-pretty-name   [attr-pretty-name default-attr-pretty-name]
                               #:struct->url        [struct->url      default-struct->url])
  (lambda (structs)
    (xml (table (@ [class 'snooze-list])
                (thead (tr ,@(for/list ([attr-name (in-list (map attr-pretty-name attributes))])
                               (xml (th ,attr-name)))))
                (tbody ,@(for/list ([struct (in-list structs)])
                           (struct->list-xml struct attr->list-renderer attributes #:struct->url struct->url)))))))

; snooze-struct (attr -> (value -> xml)) -> xml
(define (struct->list-xml struct attr->renderer attributes #:struct->url [struct->url      default-struct->url])
  (xml (tr ,@(for/list ([attr (in-list attributes)])
               ((attr->renderer attr) (snooze-struct-ref struct attr))))))

; attr -> (value -> xml)
(define (default-attr->list-renderer attr #:struct->url [struct->url default-struct->url])
  (lambda (value)
    (xml (td ,(attr->xml attr value #:struct->url struct->url)))))


; Snooze report ----------------------------------

; A mixin for a particular entity review page.
;
;  entity
;  [(attr -> (value -> xml))]
;  [#:attributes  (listof attribute)]
;  [#:struct-ref* (persistent-struct -> (listof any))]
; -> 
;  (mixin html-element<%>)
(define (entity->report-mixin snooze entity 
                              [attr->report-renderer   default-attr->report-renderer]
                              #:attr-pretty-name       [attr-pretty-name      default-attr-pretty-name]
                              #:entity->attrs          [entity->attrs         default-attributes]
                              #:query                  [query                 (default-report-query entity)]
                              #:entity->url            [entity->url           default-entity->url]
                              #:struct->url            [struct->url           default-struct->url]
                              #:plain-attr->xml        [plain-attr->xml       default-plain-attr->xml]
                              #:foreign-key-attr->xml  [foreign-key-attr->xml default-foreign-key-attr->xml])
  (mixin/cells (html-element<%>) (html-element<%>)
    
    ; Fields -----------------------------------
    
    ; (struct -> xml)
    (field report
           (entity->report snooze entity attr->report-renderer 
                           #:entity->attrs         entity->attrs
                           #:query                 query
                           #:entity->url           entity->url
                           #:struct->url           struct->url
                           #:plain-attr->xml       plain-attr->xml
                           #:foreign-key-attr->xml foreign-key-attr->xml) 
           #:child)
    
    ; Methods ------------------------------------ 
    ; seed -> xml
    (define/augment (render seed) (send report render seed))))

;  entity
;  [(attr -> (value -> xml))]
;  [#:entity->attrs (entity -> (listof attribute))]
; ->
;  ((listof struct) -> xml)
(define (entity->report snooze 
                        entity 
                        [attr->report-renderer   default-attr->report-renderer]
                        #:attr-pretty-name       [attr-pretty-name       default-attr-pretty-name]
                        #:entity->attrs          [entity->attrs          default-attributes]
                        #:query                  [query                  (default-report-query entity)]
                        #:entity->url            [entity->url            default-entity->url]
                        #:struct->url            [struct->url            default-struct->url]
                        #:plain-attr->xml        [plain-attr->xml        default-plain-attr->xml]
                        #:foreign-key-attr->xml  [foreign-key-attr->xml  default-foreign-key-attr->xml])
  (define-alias E entity)
  (let* ([attributes     (entity->attrs entity)]
         [report-columns (assemble-list
                          [(entity->url crudl:review entity) (make-column 'view-col "")]
                          [(entity->url crudl:update entity) (make-column 'edit-col "")]
                          [(entity->url crudl:delete entity) (make-column 'delete-col "")]
                          [#t                   ,@(for/list ([attr (in-list attributes)])
                                                    (let ([ATTR (sql:alias E attr)])
                                                      (make-column (attribute-name attr)
                                                                   (format "~a" (attr-pretty-name attr))
                                                                   (list (sql:asc ATTR)))))])]
         [report-filters (for/list ([attr (in-list attributes)])
                           (make-filter (attribute-name attr)
                                        (format "~a" (attr-pretty-name attr))))])
    (singleton/cells snooze-report% ()
      
      (inherit get-sort-order)
      
      (super-new [sort-col (car report-columns)])
      
      (define/override (query-num-items filter pattern)
        (find-one (sql (select #:what (count E.guid) #:from ,(query (make-where filter pattern))))))
      
      
      (define/override (query-items filter pattern col dir start count)
        (g:find (query (make-where filter pattern) (get-sort-order col dir) start count)))
      
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
                                          ((attr->report-renderer attribute 
                                                                  #:plain-attr->xml       plain-attr->xml 
                                                                  #:foreign-key-attr->xml foreign-key-attr->xml
                                                                  #:struct->url           struct->url)
                                           a-struct))])))))))))))


; attr -> (value -> xml)
(define (default-attr->report-renderer attr 
          #:plain-attr->xml       [plain-attr->xml       default-plain-attr->xml]
          #:foreign-key-attr->xml [foreign-key-attr->xml default-foreign-key-attr->xml]
          #:struct->url           [struct->url           default-struct->url])
  (lambda (struct)
    (let ([value (snooze-struct-ref struct attr)])
      (attr->xml attr value plain-attr->xml foreign-key-attr->xml #:struct->url struct->url))))



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

(provide (except-out (all-defined-out) pattern->regexp))