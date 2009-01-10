#lang scheme/base

(require (planet untyped/unlib:3/string)
         "../content-base.ss")

; Controllers ------------------------------------

; request -> response
(define-controller notification
  init-smoke-pipeline
  (lambda (request)
    (send notification-page respond)))

; Components -------------------------------------

(define notification-page
  (singleton/cells (notification-mixin html-page%) ()
    
    (inherit add-notification!
             render-notifications)
    
    ; Constructor --------------------------------
    
    (super-new)
    
    ; Methods ------------------------------------
    
    ; -> void
    (define/public #:callback (on-notify num)
      (for ([i (in-range 1 (add1 num))])
        (add-notification! (xml "Notification " ,i))))
    
    ; seed -> xml
    (define/augment (render seed)
      (xml ,(render-notifications seed)
           (ul ,@(for/list ([i (in-range 1 21)])
                   (xml (li (a (@ [href ,(embed seed (callback on-notify i))])
                               "Create " ,i ,(if (= i 1) " notification" " notifications"))))))))))
