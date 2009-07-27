#lang scheme/base

(require scheme/contract
         (prefix-in list: (only-in srfi/1 filter))
         srfi/13
         srfi/19
         (planet untyped/snooze:3/snooze)
         (planet untyped/unlib:3/list)
         (planet untyped/unlib:3/symbol)
         "../../../lib-base.ss"
         "../browser-util.ss"
         "../html-element.ss"
         "../text-field.ss"
         "report-column.ss"
         "report-filter.ss"
         "report-view.ss"
         "util.ss")

(define snooze-report%
  (class/cells html-element% ()
    
    (inherit get-id
             core-html-attributes)
    
    ; Fields -----------------------------------
    
    ; view-combo-box%
    (field view-field
      (new view-combo-box% [report this] [on-change (callback on-view-change)])
      #:child #:accessor)
    
    ; filter-combo-box%
    (field filter-field
      (new filter-combo-box% [report this] [on-change (callback on-filter-change)])
      #:child #:accessor)
    
    ; text-field%
    (field pattern-field
      (new text-field% [on-change (callback on-pattern-change)] [size 30] [placeholder "Type and press Enter to search"])
      #:child #:accessor)
    
    ; (cell (U snooze-report-column% #f))
    (init-cell sort-col #:accessor #:mutator)
    
    ; (cell (U 'asc 'desc))
    (init-cell sort-dir 'asc #:accessor #:mutator)
    
    ; (cell integer)
    (init-cell start 0 #:accessor #:mutator)
    
    ; (cell integer)
    (init-cell count 1000000 #:accessor #:mutator)
    
    ; (cell integer): [DJB] number of pages added to left and right in pager
    (init-cell pager-cell-count 4 #:accessor #:mutator)
    
    ; boolean 
    (init-cell show-controls? #t #:accessor #:mutator)
    (init-cell show-position-top? #t #:accessor #:mutator)
    (init-cell show-position-bottom? #f #:accessor #:mutator)
    (init-cell show-pager-top? #t #:accessor #:mutator)
    (init-cell show-pager-bottom? #t #:accessor #:mutator)
    
    ; Classes ----------------------------------
    
    
    ; Constructor ------------------------------
    
    ; (listof symbol)
    (init [classes null])
    
    (super-new [classes (list* 'smoke-snooze-report 'ui-widget classes)])
    
    ; (U view #f)
    ; (U filter #f)
    (init [view   (car (get-views))])
    (init [filter (if (null? (get-filters)) #f (car (get-filters)))])
    
    (when view   (send view-field set-value! view))
    (when filter (send filter-field set-value! filter))
    
    ; Miscellaneous ------------------------------
    
    ; -> boolean 
    (define/override (dirty?) 
      (or (super dirty?) 
          (send view-field dirty?) 
          (send filter-field dirty?) 
          (send pattern-field dirty?))) 
    
    ; -> (listof (U xml (seed -> xml)))
    (define/augment (get-html-requirements)
      (list* snooze-styles
             show-hide-script
             (inner null get-html-requirements)))
    
    ; symbol -> void
    (define/override (set-id! id)
      (super set-id! id)
      (send view-field set-id! (symbol-append id '-view-field))
      (send filter-field set-id! (symbol-append id '-filter-field))
      (send pattern-field set-id! (symbol-append id '-pattern-field)))
    
    ; -> symbol
    (define/public (get-table-id)
      (symbol-append (get-id) '-table))
    
    ; Queries ----------------------------------
    
    ;  [#:start (U integer #f)]
    ;  [#:count (U integer #f)]
    ; ->
    ;  integer
    ;  integer
    ;  integer
    ;  (gen-> row-data)
    (define/public (do-queries #:start [start* (get-start)] #:count [count* (get-count)])
      ; Calculate the size of the viewport and number of items:
      (define filter  (send filter-field get-value))
      (define pattern (send pattern-field get-value))
      (define total   (query-num-items filter pattern))
      (define start   (if start* (max 0 (min total start*)) #f))
      (define count   (if count* (max 0 count*) #f))
      ; Retrieve the items that are in the viewport:
      (define col     (get-sort-col))
      (define dir     (get-sort-dir))
      (define g:item  (query-items filter pattern col dir start count))
      ; Return the results:
      (values start count total g:item))
    
    ; what from where group -> integer
    (define/public (query-num-items)
      (error "query-num-items must be overridden."))
    
    ; symbol string symbol (U 'asc 'desc) integer integer -> generator
    ;
    ; start and count arepassed in pre-adjusted for the size of the
    ; viewport and the number of available items.
    (define/public (query-items filter pattern col dir start count)
      (error "query-items must be overridden."))
    
    ; [(U snooze-report-column% #f)] [(U 'asc 'desc #f)] -> (listof order)
    ; 
    ; Generate the ORDER clause for the SQL query
    (define/public (get-sort-order [col (get-sort-col)] [dir (get-sort-dir)])
      (if col (send col get-order dir) null))
    
    ; Paging -----------------------------------
    
    ; integer -> void
    (define/public #:callback (on-page start)
      (set-start! start))
    
    ; Resorting --------------------------------
    
    ; string -> void
    (define/public #:callback (on-sort id-string)
      ; symbol
      (define id (string->symbol id-string))
      ; column
      (define col
        (ormap (lambda (col)
                 (and (eq? id (send col get-id)) col))
               (get-all-columns)))
      (if (is-a? col snooze-report-column%)
          (resort! col)
          (error (format "No such column: ~s" id-string))))
    
    ; column -> void
    (define/public (resort! col)
      (define current-col (get-sort-col))
      (define current-dir (get-sort-dir))
      (if (eq? col current-col)
          (begin (set-sort-dir! (if (eq? current-dir 'asc)
                                    'desc
                                    'asc)))
          (begin (set-sort-col! col)
                 (set-sort-dir! 'asc))))
    
    ; Columns and views --------------------------
    
    ; -> (listof view)
    (define/public (get-views)
      (error "get-views must be overridden."))
    
    ; -> view
    (define/public (get-current-view)
      (send view-field get-value))
    
    ; -> view
    (define/public (set-current-view-by-id! view-identifier)
      (send view-field set-value!
            (or (for/or ([view (in-list (get-views))])
                  (and (eq? (view-id view) view-identifier) view))
                (send view-field get-value)))) 
    
    ; -> (listof column)
    (define/public (get-visible-columns)
      (list:filter (cut send <> get-display-in-html?)
                   (cond [(send view-field get-value) => (cut view-columns <>)]
                         [(car (get-views))           => (cut view-columns <>)]
                         [else                           (get-all-columns)])))
    
    ; -> (listof column)
    (define/public (get-all-columns)
      (reverse 
       (for/fold ([accum null])
                 ([view (get-views)])
                 (for/fold ([accum accum])
                           ([col (view-columns view)])
                           (if (memq col accum)
                               accum
                               (cons col accum))))))
    
    ; -> void
    (define/public #:callback (on-view-change)
      ; Don't need to do anything here. 
      ; Everything is taken care of by the combo box changing state.
      (void))
    
    ; Filters ----------------------------------
    
    ; -> (listof filter)
    (define/public (get-filters)
      (list (make-filter 'normal "Normal")))
    
    ; -> filter
    (define/public (get-current-filter)
      (if (null? (get-filters))
          #f
          (send filter-field get-value)))
    
    ; -> void
    (define/public #:callback (on-filter-change)
      (set-start! 0))
    
    ; -> void
    (define/public #:callback (on-pattern-change)
      (set-start! 0))
    
    ; Rendering ----------------------------------
    
    ; seed -> xml
    (define/override (render seed)
      ; integer integer integer (gen-> row-data)
      (define-values (start count total g:item)
        (do-queries))
      ; (listof column)
      (define cols
        (get-visible-columns))
      ; xml
      (xml (div (@ ,@(core-html-attributes seed))
                ,(opt-xml (get-show-controls?) ,(render-controls seed start count total))
                ,(opt-xml (get-show-position-top?) ,(render-position seed start count total))
                ,(opt-xml (get-show-pager-top?) ,(render-pager seed start count total))
                ; Table:
                (table (@ [id ,(get-table-id)] [class "snooze-report-table ui-widget"])
                       ,(render-head seed cols)
                       ,(if (zero? total)
                            (render-empty-body seed cols)
                            (render-body seed cols g:item))
                       ,(render-foot seed cols))
                ; Pager:
                ,(opt-xml (get-show-position-bottom?) ,(render-position seed start count total))
                ,(opt-xml (get-show-pager-bottom?) ,(render-pager seed start count total)))))
    
    ; seed integer integer integer -> xml
    (define/public (render-controls seed start count total)
      (let ([show-view-field?    (> (length (get-views)) 1)]
            [show-pattern-field? (> (length (get-filters)) 0)]
            [show-filter-field?  (> (length (get-filters)) 1)])
        (send view-field    set-visible?! show-view-field?)
        (send pattern-field set-visible?! show-pattern-field?)
        (send filter-field  set-visible?! show-filter-field?))
      (xml (div (@ [id    ,(format "~a-controls" (get-id))]
                   [class "controls ui-helper-clearfix"])
                (div (@ [class "view"])
                     ,(send view-field render seed))
                ; extra link controls:
                (div (@ [class "links"])
                     ,(render-control-links seed start count total))
                ; Filter is always visible:
                (div (@ [class "filter"])
                     ,(send filter-field render seed) " "
                     ,(send pattern-field render seed)))))
    
    ; seed integer integer integer -> xml
    (define/public (render-control-links seed start count total)
      (xml (ul (@ [class "links"])
               ,@(map (lambda (link) (xml (li ,link))) 
                      (get-control-links seed start count total))))) ; TODO replace NO-OP with ->li
    
    ; -> (listof xml) where each xml element should be an anchor (link)
    (define/public (get-control-links seed start count total)
      (list (xml (a (@ [href ,(embed/full seed (callback csv-download))]
                       [target "_new"])
                    "CSV version"))))
    
    ; seed integer integer integer -> xml
    (define/public (render-position seed start count total)
      (xml (div (@ [class "position ui-helper-clearfix"])
                ,(opt-xml (not (zero? total))
                   (div (@ [class "item-count"])
                        "Displaying items " ,(max 1 (min total (if start (add1 start) 1)))
                        " to " ,(max 1 (min total (cond [(and start count) (+ start count)]
                                                        [count count]
                                                        [else total])))
                        " of " ,total ".")))))
    
    ; seed integer integer integer -> xml
    (define/public (render-pager seed start count total)
      ;  integer
      ;  (U integer string)
      ;  [#:title (U string #f)]
      ;  [#:active?   boolean]
      ;  [#:disabled? boolean]
      ; ->
      ;  xml
      (define (link to text #:title [title #f] #:active? [active? #f] #:disabled? [disabled? #f])
        (xml (td (@ [class ,(cond [active?   'ui-state-active]
                                  [disabled? "ui-state-default ui-state-disabled"]
                                  [else      'ui-state-default])]
                    ,(opt-xml-attr title))
                 (a (@ [onclick ,(embed/ajax seed (callback on-page to))]) ,text))))
      
      ; [DJB] page windowing
      ; integer integer -> (values integer integer)
      (define (calculate-pager-limits current-page last-page)
        (let* ([window-offset       (get-pager-cell-count)]
               [unbounded-low-page  (- current-page window-offset)]
               [bounded-low-page    (max unbounded-low-page 0)]
               [unbounded-high-page (add1 (+ current-page window-offset))]
               [bounded-high-page   (min unbounded-high-page last-page)])
          
          ; integer: pager-window index of first page to display
          ; if there are unused window cells on high end, add to low
          (define window-first-page
            (let ([take-from-low (- unbounded-high-page bounded-high-page)] )
              (max (- bounded-low-page take-from-low) 0)))
          
          ; integer: pager-window index of last page to display
          ; if there are unused window cells on low end, add to high
          (define window-last-page
            (let ([add-to-high   (- bounded-low-page unbounded-low-page)])
              (min (+ bounded-high-page add-to-high) last-page)))
          
          (values window-first-page window-last-page)))
      
      ; integer: index (within report) of item that starts last page
      (define last-item (max 0 (- (sub1 total) (remainder (sub1 total) count))))
      
      ; integer: index (within pager) of last page
      (define last-page (add1 (ceiling (/ last-item count))))
      
      ; integer: index (within page) of current page
      (define current-page  (floor (/ start count)))
      
      ; integers: page-index limits of pager-window
      (define-values (pager-first-page pager-last-page)
        (calculate-pager-limits current-page last-page))
      
      ; integers: indices (within report) of first-page first-item; last-page first-item
      (define lowest-item  (max (* pager-first-page count) 0))
      (define highest-item (min (* pager-last-page count) total))      
      
      ; xml      
      ; If we are displaying all items in the list, there is no need for a pager:
      (opt-xml (not (or (zero? total) (not count) (>= count total)))
        ; Otherwise, output a list of links like this:
        ;   < Prev 1 2 Next >
        ; for each page that can be viewed.
        (table (@ [class "pager"]) 
               (tr ,(link 0 (xml (& laquo))                    ; formerly 'fixed
                          #:title "Jump to page 1"
                          #:disabled? (zero? current-page))
                   ,(link (max 0 (- start count))              ; formerly 'fixed
                          (xml (& lsaquo)) 
                          #:title "Previous page"
                          #:disabled? (zero? current-page))
                   ; ellipsis
                   ,(if (not (zero? pager-first-page)) 
                        (xml (td (@ [class 'spacer]) (span "...")))
                        (xml (td (@ [class 'spacer]) (span (& nbsp)))))
                   ,@(for/list ([i (in-range lowest-item highest-item count)])
                       (link i (add1 (/ i count)) #:active? (and (>= i start) (< i (+ start count)))))
                   ; ellipsis
                   ,(if (not (= last-page pager-last-page)) 
                        (xml (td (@ [class 'last]) (span "...")))
                        (xml (td (@ [class 'last]) (span (& nbsp)))))
                   ,(link (min last-item (+ start count))      ; formerly 'fixed
                          (xml (& rsaquo))
                          #:title "Next page"
                          #:disabled? (not (< current-page (sub1 last-page))))
                   ,(link last-item (xml (& raquo))            ; formerly 'fixed
                          #:title (string-append "Jump to page " (number->string last-page))
                          #:disabled? (not (< current-page (sub1 last-page))))))))
    
    ; seed (listof column) -> xml
    (define/public (render-head seed cols)
      ; column
      (define current-col (get-sort-col))
      ; (U 'asc 'desc)
      (define current-dir (get-sort-dir))
      ; xml
      (xml (thead (tr (@ [class 'ui-widget-header])
                      ,@(for/list ([col (in-list (get-visible-columns))])
                          (send col render-head seed (and (equal? col current-col) current-dir)))))))
    
    ; seed (listof column) -> xml
    (define/public (render-empty-body seed cols)
      (xml (tbody (@ [class "ui-widget-content"])
                  (tr (td (@ [colspan ,(length cols)]
                             [class "empty-row"])
                          "There are no items to display in this list.")))))
    
    ; seed (listof column) (gen-> item) -> xml
    (define/public (render-body seed cols g:item)
      (xml (tbody (@ [class "ui-widget-content"])
                  ,@(g:collect (g:map (cut render-item seed cols <>) g:item)))))
    
    ; seed (listof column) item -> xml
    (define/public (render-item seed cols item)
      (error "render-item must be overwritten"))
    
    ; seed (listof column) -> xml
    (define/public (render-foot seed cols)
      (xml))
    
    ; CSV ----------------------------------------
    
    ; -> (listof column)
    (define/public (get-csv-columns)
      (list:filter (cut send <> get-display-in-csv?)
                   (get-all-columns)))
    
    ; string -> void
    (define/public #:callback (csv-download)
      (respond/csv))
    
    ; -> string
    (define/public (get-csv-download-filename)
      (format "download-~a.csv" (date->string (current-date) "~Y-~m-~d-~H-~M-~S")))
    
    ; [string] -> string
    (define/public (get-csv-download-headers [download-filename (get-csv-download-filename)])
      ; string
      (define content-disposition-string
        (format "attachment; filename=~a" download-filename))
      ; header
      (define content-disposition-header
        (make-header #"Content-Disposition" (string->bytes/utf-8 content-disposition-string)))
      ; header
      (define content-type-header
        (make-header #"Content-Type" #"text/csv"))
      (list* content-disposition-header
             content-type-header
             no-cache-http-headers))
    
    ; [(listof column)] -> void
    (define/public (respond/csv [cols (get-csv-columns)])
      ; void
      (send/suspend/dispatch
       (lambda (embed-url)
         (make-csv-response
          #:headers (get-csv-download-headers)
          (render/csv cols)))))
    
    ; [(listof column)] -> csv:sheet
    (define/public (render/csv [cols (get-csv-columns)])
      ; integer integer integer (gen-> row-data)
      (define-values (start count total g:item)
        (do-queries #:start #f #:count #f))
      (csv:sheet (render-head/csv cols)
                 (render-body/csv cols g:item)
                 (render-foot/csv cols)))
    
    ; (listo column) -> (treeof csv:row)
    (define/public (render-head/csv cols)
      (csv:row (map (cut send <> render-head/csv) cols)))
    
    ; (listof column) (gen-> item) -> (treeof csv:row)
    (define/public (render-body/csv cols g:item)
      (g:collect (g:map (cut render-item/csv cols <>) g:item)))
    
    ; (listof column) item -> (treeof csv:row)
    (define/public (render-item/csv cols item)
      (error "render-item/csv must be overridden."))
    
    ; (listof column) -> (treeof csv:row)
    (define/public (render-foot/csv cols)
      null)
    
    ; Attach and detach --------------------------
    
    ; seed -> js
    (define/augride (get-on-attach seed)
      (js ,@(filter-map 
             (lambda (col)
               (let ([sort-id (send col get-sort-id)]) ; (U symbol #f)
                 ; Unsortable columns need no attaching:
                 (and sort-id (js (!dot ($ ,(format "#~a" sort-id))
                                        (click (function ()
                                                 ,(embed/ajax seed (callback on-sort (symbol->string (send col get-id)))))))
                                  (!dot ($ ,(format "#~a .ui-state-default:not(.ui-state-disabled):not(.not-sortable)" 
                                                    (get-id)))
                                        (hover (function (event ui)
                                                 (!dot ($ this) (addClass "ui-state-hover")))
                                               (function (event ui)
                                                 (!dot ($ this) (removeClass "ui-state-hover")))))))))
             (get-visible-columns))))
    
    ; seed -> js
    (define/augride (get-on-detach seed)
      (js ,@(filter-map (lambda (col)
                          (let ([sort-id (send col get-sort-id)]) ; (U symbol #f)
                            ; Unsortable columns need no attaching:
                            (and sort-id (js (!dot ($ ,(format "#~a" sort-id))
                                                   (unbind))))))
                        (get-visible-columns))))))

; Provide statements --------------------------- 

(provide (except-out (all-from-out "report-column.ss"
                                   "report-filter.ss"
                                   "report-view.ss")
                     filter
                     view)
         snooze-report%)
