#lang web-server

(require "../content-base.ss")

; Controllers ------------------------------------

; request -> response
(define-controller (redirect)
  (send redirect-page respond))

; Components -------------------------------------

(define redirect-page
  (singleton/cells html-page% ()
    
    ; seed -> xml
    (define/augment (render seed)
      (xml (p (a (@ [href    ,(embed/full seed (callback on-redirect))]) "Full refresh"))
           (p (a (@ [onclick ,(embed/ajax seed (callback on-redirect))]) "AJAX refresh"))))
    
    (define/public #:callback (on-redirect)
      (redirect-to (controller-url home)))))
