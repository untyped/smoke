#lang scheme/base

(require (for-syntax scheme/base)
         scheme/runtime-path
         (planet untyped/unlib:3/yield)
         "../content-base.ss")

; Controllers ------------------------------------

; request -> response
(define-controller test-current-request
  `(,@init-smoke-pipeline
    ,(lambda (continue . args)
       ; Check the request:
       (if (request? (current-request))
           (apply continue args)
           (error "invalid current-request" (current-request)))))
  (lambda (request)
    (send test-page respond)))

; Components -------------------------------------

(define test-page
  (singleton/cells html-page% ()
    
    ; Constructor --------------------------------
    
    (super-new [title "Current request test"])
    
    ; Methods ------------------------------------
    
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
      (void))))
