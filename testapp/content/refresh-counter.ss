#lang scheme/base

(require "../content-base.ss")

; Controllers ------------------------------------

; request -> response
(define-controller (refresh-counter request)
  (send refresh-counter-page respond))

; Components -------------------------------------

; TODO : Potential problem:
; 1. Load page
; 2. Click "Add1"
; 3. Remove continuation code and reload page

(define refresh-counter-page
  (singleton/cells html-page% ()
    
    ; Fields -------------------------------------
    
    (cell counter 0 #:accessor #:mutator)
    
    (super-new [title "Refresh counter"])
    
    ; Methods ------------------------------------
    
    ; seed -> xml
    (define/augment (render seed)
      (xml (p "Counter: " (span (@ [id 'counter]) ,(get-counter)))
           (p (a (@ [id "ajax"] [onclick ,(embed/ajax seed (callback on-refresh))]) "[Add1]"))
           ,(inner (xml) render seed)))
      
    ; -> void
    (define/public #:callback (on-refresh)
      (void))
    
    (define/augment (get-on-attach seed)
      (begin0 (inner (js) get-on-attach seed)
              (set-counter! (add1 (get-counter)))))))

; Provide statements -----------------------------

(provide refresh-counter-page)
