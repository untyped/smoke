#lang scheme/base

(require "../content-base.ss")

(define-page redirect-page html-page% ()
    
  ; seed -> xml
  (define/augment (render seed)
    (xml (p (a (@ [href    ,(embed/full seed (callback on-redirect))]) "Full refresh"))
         (p (a (@ [onclick ,(embed/ajax seed (callback on-redirect))]) "AJAX refresh"))))
  
  (define/public #:callback (on-redirect)
    (redirect-to (controller-url home-page))))
