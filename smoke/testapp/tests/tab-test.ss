#lang scheme/base

(require srfi/13/string
         "../../test-base.ss")

; Helpers ----------------------------------------

; integer -> void
(define (select-tab num)
  (click/wait (node/xpath (format "//ul[@class='labels']/li[~a]/a" (add1 num)))))

; Tests ------------------------------------------

(define tab-tests
  (test-suite "tab"
    
    (test-case "test-page displays"
      (open/wait "/tab")
      (check-equal? (title-ref) "Tab pane"))
    
    (test-case "initial editor content"
      (check-equal? (js-ref (js (!dot Smoke (findById "editor") value))) "Content"))
    
    (test-case "initial preview content"
      (select-tab 1)
      (check-equal? (inner-html-ref (node/tag 'div (node/class 'current-tab))) "Content")
      (reload/wait)
      (check-equal? (inner-html-ref (node/tag 'div (node/class 'current-tab))) "Content"))
    
    (test-case "enter new content"
      (select-tab 0)
      (enter-text (node/id 'editor) "New content")
      (check-equal? (js-ref (js (!dot Smoke (findById "editor") value))) "New content")
      (reload/wait)
      (check-equal? (js-ref (js (!dot Smoke (findById "editor") value))) "New content"))
    
    (test-case "initial preview content"
      (select-tab 1)
      (check-equal? (inner-html-ref (node/tag 'div (node/class 'current-tab))) "New content")
      (reload/wait)
      (check-equal? (inner-html-ref (node/tag 'div (node/class 'current-tab))) "New content"))))

; Provide statements -----------------------------

(provide tab-tests)

