#lang scheme/base

(require "../content-base.ss")

(define-page refresh-counter-page html-page% ()
  
  ; Fields ---------------------------------------
  
  (cell counter 0 #:accessor #:mutator)
  
  (super-new [title "Refresh counter"])
  
  ; Methods --------------------------------------
  
  ; seed -> xml
  (define/augment (render seed)
    (xml (p "Counter: " (span (@ [id 'counter]) ,(get-counter)))
         (p (a (@ [id "ajax"] [onclick ,(embed/ajax seed (callback on-refresh))]) "[Add1]"))
         ,(inner (xml) render seed)))
  
  ; -> void
  ; This is already defined in html-page% for ajax->full redirects:
  ; (define/public #:callback (on-refresh)
  ;   (void))
  
  (define/augment (get-on-attach seed)
    (begin0 (inner (js) get-on-attach seed)
            (set-counter! (add1 (get-counter))))))
