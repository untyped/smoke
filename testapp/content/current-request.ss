#lang scheme

(require "../content-base.ss")

; Components -------------------------------------

(define-page current-request-page html-page% ()
  
  ; Constructor ----------------------------------
  
  (super-new [title "Current request test"])
  
  ; Methods --------------------------------------
  
  ; seed -> xml
  (define/augment (render seed)
    ; Check the request:
    (unless (request? (current-request))
      (error "current-request is #f"))
    (xml (p (a (@ [id 'ajax] [onclick ,(embed/ajax seed (callback on-click))])
               "AJAX refresh"))))
  
  ; -> void
  (define/public #:callback (on-click)
    ; Check the request:
    (unless (request? (current-request))
      (error "current-request is #f"))
    (void)))
