#lang web-server

(require "../../lib-base.ss"
         "../../test-base.ss")

; Custom checks ----------------------------------

; integer -> void
(define-check (check-reload counter)
  (with-check-info (['reload "no"])
    (check-equal? (inner-html-ref (node/id 'counter)) (number->string counter)))
  (reload/wait)
  (with-check-info (['reload "yes"])
    (check-equal? (inner-html-ref (node/id 'counter)) (number->string counter))))

; Tests ------------------------------------------

(define counter-tests
  (test-suite "counter"
    
    (test-case "full refresh"
      (open/wait "/counter")
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
    
    (test-case "ajax thunk"
      (open/wait "/counter")
      (check-reload 0 "A: initial")
      (click/wait (node/id 'ajax-add1))
      (check-reload 1 "B: ajax")
      (click/wait (node/id 'full-add1))
      (check-reload 2 "C: ajax -> full")
      (click/wait (node/id 'ajax-add1))
      (check-reload 3 "D: ajax -> full -> ajax")
      (back/wait)
      (check-reload 1 "E: ajax -> full -> ajax -> back")
      (click/wait (node/id 'ajax-sub1))
      (check-reload 0 "F: ajax -> full -> ajax -> back -> ajax")
      (click/wait (node/id 'full-sub1))
      (check-reload -1 "G: ajax -> full -> ajax -> back -> full")
      (click/wait (node/id 'ajax-sub1))
      (check-reload -2 "H: ajax -> full -> ajax -> back -> full -> ajax"))
    
    (test-case "ajax callback"
      (open/wait "/counter")
      (check-reload 0 "initial")
      (click/wait (node/id 'callback-add1))
      (check-reload 1 "callback")
      (click/wait (node/id 'full-add1))
      (check-reload 2 "callback -> full")
      (click/wait (node/id 'callback-add1))
      (check-reload 3 "callback -> full -> callback")
      (back/wait)
      (check-reload 1)
      (click/wait (node/id 'callback-sub1))
      (check-reload 0)
      (click/wait (node/id 'full-sub1))
      (check-reload -1)
      (click/wait (node/id 'callback-sub1))
      (check-reload -2))))

; Provide statements -----------------------------

(provide counter-tests)

