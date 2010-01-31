#lang scheme

(require "../content-base.ss")

; Components -------------------------------------

(define-page counter-page html-page% ()
  
  ; Fields ---------------------------------------
  
  (cell counter 0 #:accessor #:mutator)
  
  ; Constructor ----------------------------------
  
  (super-new [title "Counter"])
  
  ; Methods --------------------------------------
    
  ; seed -> xml
  (define/augment (render seed)
    (xml (p "Counter: " (span (@ [id 'counter]) ,(get-counter)) " "
            (a (@ [id "full-add1"] [href ,(embed/full seed (callback on-increment))]) "[Full add1]") " "
            (a (@ [id "full-sub1"] [href ,(embed/full seed (callback on-decrement))]) "[Full sub1]") " "
            (a (@ [id "ajax-add1"] [href "javascript:;"] [onclick ,(embed/ajax seed (callback on-increment))]) "[AJAX add1]") " "
            (a (@ [id "ajax-sub1"] [href "javascript:;"] [onclick ,(embed/ajax seed (callback on-decrement))]) "[AJAX sub1]"))))
  
  ; -> void
  (define/public #:callback (on-increment)
    (set-counter! (add1 (get-counter))))
  
  ; -> void
  (define/public #:callback (on-decrement)
    (set-counter! (sub1 (get-counter)))))
