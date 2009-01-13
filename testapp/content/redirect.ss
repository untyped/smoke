#lang scheme/base

(require "../content-base.ss")

; Controllers ------------------------------------

; -> response
(define-controller redirect
  init-smoke-pipeline
  (lambda ()
    (send redirect-page respond)))

; Components -------------------------------------

(define redirect-page
  (singleton/cells html-page% ()
    
    ; seed -> xml
    (define/augment (render seed)
      (xml (p (a (@ [href    ,(embed/full seed (callback on-redirect))]) "Full refresh"))
           (p (a (@ [onclick ,(embed/ajax seed (callback on-redirect))]) "AJAX refresh"))))
    
    (define/public #:callback (on-redirect)
      (redirect-to (controller-url home)))))
