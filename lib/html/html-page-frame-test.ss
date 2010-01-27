#lang web-server

(require (only-in srfi/13/string string-trim-both)
         "../../lib-base.ss"
         "../../test-base.ss"
         "html-component.ss"
         "html-page.ss")

; Custom checks ----------------------------------

; integer -> void
(define-check (check-reload counter)
  (check-equal? (inner-html-ref (node/id 'counter)) (number->string counter))
  (reload/wait)
  (check-equal? (inner-html-ref (node/id 'counter)) (number->string counter)))

; Tests ------------------------------------------

(define html-page-frame-tests
  (test-suite "html-page.ss : frames"
    
    (test-case "full refresh"
      (open/page counter-page)
      (check-reload 0)
      (click/wait (node/id 'full-add1))
      (check-reload 1)
      (click/wait (node/id 'full-add1))
      (check-reload 2)
      (click/wait (node/id 'full-add1))
      (check-reload 3)
      (back/wait)
      (check-reload 2)
      (back/wait)
      (check-reload 1)
      (click/wait (node/id 'full-sub1))
      (check-reload 0)
      (click/wait (node/id 'full-sub1))
      (check-reload -1))
    
    (test-case "ajax"
      (open/page counter-page)
      (check-reload 0 "initial")
      (click/wait (node/id 'ajax-add1))
      (check-reload 1 "ajax")
      (click/wait (node/id 'full-add1))
      (check-reload 2 "ajax -> full")
      (click/wait (node/id 'ajax-add1))
      (check-reload 3 "ajax -> full -> ajax")
      (back/wait)
      (check-reload 1)
      (click/wait (node/id 'ajax-sub1))
      (check-reload 0)
      (click/wait (node/id 'full-sub1))
      (check-reload -1)
      (click/wait (node/id 'ajax-sub1))
      (check-reload -2))))

; Helpers ----------------------------------------

(define counter-page
  (singleton/cells html-page% ()
    
    ; Fields -------------------------------------
    
    (cell counter 0 #:accessor #:mutator)
    
    (super-new [title "Counter"])
    
    ; Methods ------------------------------------
    
    ; seed -> xml
    (define/override (render seed)
      (xml (p "Counter: " (span (@ [id 'counter]) ,(get-counter)) " "
              (a (@ [id "full-add1"] [href ,(embed seed (cut on-increment))]) "[Full add1]") " "
              (a (@ [id "full-sub1"] [href ,(embed seed (cut on-decrement))]) "[Full sub1]") " "
              (a (@ [id "ajax-add1"] [onclick ,(embed/ajax seed (cut on-increment))]) "[AJAX add1]") " "
              (a (@ [id "ajax-sub1"] [onclick ,(embed/ajax seed (cut on-decrement))]) "[AJAX sub1]") " "
              #;(a (@ [id "callback-add1"] [onclick ,(embed/ajax seed (callback on-increment))]) "[Callback add1]") " "
              #;(a (@ [id "callback-sub1"] [onclick ,(embed/ajax seed (callback on-decrement))]) "[Callback sub1]"))))
    
    ; -> void
    (define/public #:callback (on-increment)
      (set-counter! (add1 (get-counter))))
    
    ; -> void
    (define/public #:callback (on-decrement)
      (set-counter! (sub1 (get-counter))))))

; Provide statements -----------------------------

(provide html-page-frame-tests)

