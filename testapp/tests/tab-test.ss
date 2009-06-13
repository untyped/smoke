#lang scheme/base

(require srfi/13/string
         "../../test-base.ss")

; Tests ------------------------------------------

(define tab-tests
  (test-suite "tab"
    
    (test-case "test-page displays"
      (open/wait "/tab")
      (check-equal? (title-ref) "Tab pane"))
    
    (test-case "initial tabs"
      (for ([id (in-list '(tab-pane
                           tab1 editor
                           tab2 inline-preview
                           tab4 nested-tab-pane
                           tab41 nested-inline-preview))])
        (with-check-info (['id id])
          (check-true (node-exists? (node/id id)))))
      (for ([id (in-list '(tab3 demand-preview tab42 nested-demand-preview))])
        (with-check-info (['id id])
          (check-false (node-exists? (node/id id))))))
    
    (test-case "initial content"
      (check-equal? (js-ref (js (!dot ($ "#editor") (val)))) "Content")
      (check-equal? (inner-html-ref (node/id 'inline-preview)) "Content")
      (check-equal? (inner-html-ref (node/id 'nested-inline-preview)) "Content"))
        
    (test-case "enter new content"
      (enter-text (node/id 'editor) "New content")
      (reload/wait)
      (read-line)
      (check-equal? (js-ref (js (!dot ($ "#editor") (val)))) "New content")
      (check-equal? (inner-html-ref (node/id 'inline-preview)) "New content")
      (check-equal? (inner-html-ref (node/id 'nested-inline-preview)) "New content"))
    
    (test-case "select tabs"
      (click/wait (node/jquery "#tab-pane ul li:eq(2)"))
      (check-true (node-exists? (node/id 'tab3)))
      (check-true (node-exists? (node/id 'demand-preview)))
      (check-equal? (inner-html-ref (node/id 'demand-preview)) "New content"))
    ))

; Provide statements -----------------------------

(provide tab-tests)

