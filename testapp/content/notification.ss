#lang scheme/base

(require (planet untyped/unlib:3/string)
         "../content-base.ss")

; Components -------------------------------------

(define notification-page%
  (class/cells (notification-mixin html-page%) ()
    
    (inherit add-notification!
             render-notifications)
    
    ; Fields -------------------------------------
    
    (init-field page-number)
    
    ; Constructor --------------------------------
    
    (super-new)
    
    ; Methods ------------------------------------
    
    ; -> void
    (define/public #:callback (on-notify num sticky?)
      (for ([i (in-range 1 (add1 num))])
        (add-notification! (xml ,(if sticky? 
                                     "Sticky notification "
                                     "Notification ") ,i)
                           sticky?)))
    
    ; seed -> xml
    (define/augment (render seed)
      (xml (h1 "Notification page " ,page-number)
           (p (a (@ [href ,(controller-url notification1)]) "Visit page 1") " "
              (a (@ [href ,(controller-url notification2)]) "Visit page 2"))
           ,(render-notifications seed)
           (ul ,@(for/list ([i (in-range 1 4)])
                   (xml (li (a (@ [id ,(format "normal-~a" i)] [href ,(embed seed (callback on-notify i #f))])
                               "Create " ,i ,(if (= i 1) " notification" " notifications")))))
               ,@(for/list ([i (in-range 1 4)])
                   (xml (li (a (@ [id ,(format "sticky-~a" i)] [href ,(embed seed (callback on-notify i #t))])
                               "Create " ,i ,(if (= i 1) " sticky notification" " sticky notifications"))))))))))

(define notification-page1
  (new notification-page% [page-number 1]))

(define notification-page2
  (new notification-page% [page-number 2]))

; Controllers ------------------------------------

; -> response
(define-controller notification1
  init-smoke-pipeline
  (lambda ()
    (send notification-page1 respond)))

; -> response
(define-controller notification2
  init-smoke-pipeline
  (lambda ()
    (send notification-page2 respond)))
