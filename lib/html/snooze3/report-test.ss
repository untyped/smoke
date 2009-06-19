#lang scheme/base

(require "../../../test-base.ss"
         (prefix-in list: (only-in srfi/1/list filter))
         (planet untyped/unlib:3/gen)
         "report.ss")

; Tests ------------------------------------------

(define report-tests
  (test-suite "report.ss"
    
    (test-case "test-page opens"
      (open/page test-page))
    
    (test-case "ordered by symbol"
      (check-equal? (js-ref (js (!dot Smoke (findById "report-table") rows length))) 6)
      (check-equal? (inner-html-ref (node/cell/xy 0 1 (node/id 'report-table))) "a")
      (check-equal? (inner-html-ref (node/cell/xy 1 1 (node/id 'report-table))) "a")
      (check-equal? (inner-html-ref (node/cell/xy 0 2 (node/id 'report-table))) "f")
      (check-equal? (inner-html-ref (node/cell/xy 1 2 (node/id 'report-table))) "b"))
    
    (test-case "order by string"
      (click/wait (node/id 'report-string-sort))
      (check-equal? (js-ref (js (!dot Smoke (findById "report-table") rows length))) 6)
      (check-equal? (inner-html-ref (node/cell/xy 0 1 (node/id 'report-table))) "a")
      (check-equal? (inner-html-ref (node/cell/xy 1 1 (node/id 'report-table))) "a")
      (check-equal? (inner-html-ref (node/cell/xy 0 2 (node/id 'report-table))) "b")
      (check-equal? (inner-html-ref (node/cell/xy 1 2 (node/id 'report-table))) "c"))
    
    (test-case "change view"
      (select/wait (node/id 'report-view-field) "view2")
      (check-equal? (js-ref (js (!dot Smoke (findById "report-view-field") value))) "view2")
      (check-equal? (js-ref (js (!dot Smoke (findById "report-table") rows length))) 6)
      (check-equal? (inner-html-ref (node/cell/xy 0 1 (node/id 'report-table))) "a")
      (check-equal? (inner-html-ref (node/cell/xy 1 1 (node/id 'report-table))) "10")
      (check-equal? (inner-html-ref (node/cell/xy 0 2 (node/id 'report-table))) "c")
      (check-equal? (inner-html-ref (node/cell/xy 1 2 (node/id 'report-table))) "9")
      (select/wait (node/id 'report-view-field) "view3")
      (check-equal? (js-ref (js (!dot Smoke (findById "report-view-field") value))) "view3")
      (check-equal? (js-ref (js (!dot Smoke (findById "report-table") rows length))) 6)
      (check-equal? (inner-html-ref (node/cell/xy 0 1 (node/id 'report-table))) "a")
      (check-equal? (inner-html-ref (node/cell/xy 1 1 (node/id 'report-table))) "10")
      (check-equal? (inner-html-ref (node/cell/xy 0 2 (node/id 'report-table))) "b")
      (check-equal? (inner-html-ref (node/cell/xy 1 2 (node/id 'report-table))) "9"))
    
    (test-case "change filter"
      (enter-text/wait (node/id 'report-pattern-field) "b")
      (check-equal? (js-ref (js (!dot Smoke (findById "report-table") rows length))) 2)
      (check-equal? (inner-html-ref (node/cell/xy 0 1 (node/id 'report-table))) "b")
      (check-equal? (inner-html-ref (node/cell/xy 1 1 (node/id 'report-table))) "9")
      (select/wait (node/id 'report-filter-field) "filter2")
      (check-equal? (js-ref (js (!dot Smoke (findById "report-table") rows length))) 2)
      (check-equal? (inner-html-ref (node/cell/xy 0 1 (node/id 'report-table))) "f")
      (check-equal? (inner-html-ref (node/cell/xy 1 1 (node/id 'report-table))) "5")
      (select/wait (node/id 'report-filter-field) "filter3")
      (check-equal? (js-ref (js (!dot Smoke (findById "report-table") rows length))) 2)
      (check-not-false (regexp-match #rx"no items" (inner-html-ref (node/cell/xy 0 1 (node/id 'report-table)))))
      (select/wait (node/id 'report-filter-field) "filter1")
      (check-equal? (js-ref (js (!dot Smoke (findById "report-table") rows length))) 2)
      (check-equal? (inner-html-ref (node/cell/xy 0 1 (node/id 'report-table))) "b")
      (check-equal? (inner-html-ref (node/cell/xy 1 1 (node/id 'report-table))) "9")
      (enter-text/wait (node/id 'report-pattern-field) "")
      (check-equal? (js-ref (js (!dot Smoke (findById "report-table") rows length))) 6)
      (check-equal? (inner-html-ref (node/cell/xy 0 1 (node/id 'report-table))) "a")
      (check-equal? (inner-html-ref (node/cell/xy 1 1 (node/id 'report-table))) "10")
      (check-equal? (inner-html-ref (node/cell/xy 0 2 (node/id 'report-table))) "b")
      (check-equal? (inner-html-ref (node/cell/xy 1 2 (node/id 'report-table))) "9"))
    
    (test-case "page"
      (let ([check-page1
             (lambda ()
               (check-equal? (js-ref (js (!dot Smoke (findById "report-table") rows length))) 6)
               (check-equal? (inner-html-ref (node/cell/xy 0 1 (node/id 'report-table))) "a")
               (check-equal? (inner-html-ref (node/cell/xy 1 1 (node/id 'report-table))) "10")
               (check-equal? (inner-html-ref (node/cell/xy 0 2 (node/id 'report-table))) "b")
               (check-equal? (inner-html-ref (node/cell/xy 1 2 (node/id 'report-table))) "9"))]
            [check-page2
             (lambda ()
               (check-equal? (js-ref (js (!dot Smoke (findById "report-table") rows length))) 6)
               (check-equal? (inner-html-ref (node/cell/xy 0 1 (node/id 'report-table))) "f")
               (check-equal? (inner-html-ref (node/cell/xy 1 1 (node/id 'report-table))) "5")
               (check-equal? (inner-html-ref (node/cell/xy 0 2 (node/id 'report-table))) "g")
               (check-equal? (inner-html-ref (node/cell/xy 1 2 (node/id 'report-table))) "4"))])
        (click/wait (node/first (node/link/text "2")))
        (check-page2)
        (click/wait (node/first (node/link/text "1")))
        (check-page1)
        (click/wait (node/first (node/link/text ">")))
        (check-page2)
        (click/wait (node/first (node/link/text "<")))
        (check-page1)))))

; Helpers ----------------------------------------

; column
(define string-col (make-column 'string "String" #t))
(define symbol-col (make-column 'symbol "Symbol" #t))
(define number-col (make-column 'number "Number" #t))

; view
(define view1 (make-view 'view1 "View 1" (list string-col symbol-col)))
(define view2 (make-view 'view2 "View 2" (list symbol-col number-col)))
(define view3 (make-view 'view3 "View 3" (list string-col number-col)))

(define filter1 (make-filter 'filter1 "Filter 1"))
(define filter2 (make-filter 'filter2 "Filter 2"))
(define filter3 (make-filter 'filter3 "Filter 3"))

(define test-report%
  (class/cells snooze-report% ()
    
    ; Constructor ------------------------------
    
    ; column
    (super-new [sort-col symbol-col] [count 5])
    
    ; SQL --------------------------------------
    
    ; filter any -> integer
    (define/override (query-num-items filter pattern)
      (length (g:collect (query-items filter pattern string-col 'asc 0 999999))))
    
    ; filter any column (U 'asc 'desc) integer integer -> (gen-> item)
    (define/override (query-items filter pattern col dir start count)
      (items-ref filter pattern col dir start count))
    
    ; Columns and views ------------------------
    
    ; -> (listof view)
    (define/override (get-views)
      (list view1 view2 view3))
    
    ; Filters ----------------------------------
    
    ; -> (listof filter)
    (define/override (get-filters)
      (list filter1 filter2 filter3))
    
    ; Rendering --------------------------------
    
    ; seed (listof column) (list string symbol number) -> xml
    (define/override (render-item seed cols item)
      (match item
        [(list str sym num)
         (xml (tr ,@(map (lambda (col)
                           (cond [(eq? col string-col) (xml (td ,str))]
                                 [(eq? col symbol-col) (xml (td ,sym))]
                                 [(eq? col number-col) (xml (td ,num))]))
                         cols)))]))
    
    ; CSV ----------------------------------------
    
    ; (listof column) (list string symbol number) -> xml
    (define/override (render-item/csv cols item)
      (match item
        [(list str sym num)
         (csv:row (map (lambda (col)
                         (csv:cell (cond [(eq? col string-col) str]
                                         [(eq? col symbol-col) sym]
                                         [(eq? col number-col) num])))
                       cols))]))))

; html-page%
(define test-page
  (singleton/cells html-page% ()
    
    ; Fields -------------------------------------
    
    (field report
      (new test-report% [id 'report])
      #:child #:accessor #:mutator)
    
    ; Constructor --------------------------------
    
    (super-new [id 'page])))

; filter any column (U 'asc 'desc) integer integer -> (gen-> item)
(define (items-ref filter pattern sort-col sort-dir start count)
  (list->generator 
   (safe-take
    (safe-drop
     (list:filter
      (cond [(eq? filter filter1)
             (lambda (item)
               (or (not pattern) (equal? (car item) pattern)))]
            [(eq? filter filter2)
             (lambda (item)
               (or (not pattern) (equal? (symbol->string (cadr item)) pattern)))]
            [(eq? filter filter3)
             (lambda (item)
               (or (not pattern) (equal? (number->string (caddr item)) pattern)))]
            [else (lambda (item) #t)])
      (sort 
       (list (list "a" 'a 10)
             (list "b" 'c 9)
             (list "c" 'e 8)
             (list "d" 'g 7)
             (list "e" 'i 6)
             (list "f" 'b 5)
             (list "g" 'd 4)
             (list "h" 'f 3)
             (list "i" 'h 2)
             (list "j" 'j 1))
       (cond [(eq? sort-col string-col)
              (if (eq? sort-dir 'asc)
                  (lambda (a b)
                    (string>? (car b)
                              (car a)))
                  (lambda (a b)
                    (string>? (car a) 
                              (car b))))]
             [(eq? sort-col symbol-col)
              (if (eq? sort-dir 'asc)
                  (lambda (a b)
                    (string>? (symbol->string (cadr b))
                              (symbol->string (cadr a))))
                  (lambda (a b)
                    (string>? (symbol->string (cadr a))
                              (symbol->string (cadr b)))))]
             [(eq? sort-col number-col)
              (if (eq? sort-dir 'asc)
                  (lambda (a b)
                    (> (caddr b)
                       (caddr a)))
                  (lambda (a b)
                    (> (caddr a)
                       (caddr b))))]
             [else (lambda (a b) #t)])))
     start)
    count)))

; list integer -> list
(define (safe-drop lst num)
  (cond [(not num) lst]
        [(> num (length lst)) null]
        [else (drop lst num)]))

; list integer -> list
(define (safe-take lst num)
  (cond [(not num) lst]
        [(> num (length lst)) lst]
        [else (take lst num)]))

; Provide statements -----------------------------

(provide snooze-report-tests)
