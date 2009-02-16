#lang scheme/base

(require (planet untyped/unlib:3/string)
         "../content-base.ss")

; Components -------------------------------------

(define notification-page%
  (class/cells html-page% ()
    
    ; Fields -------------------------------------
    
    (init-field page-number)
    
    (field notification-pane
      (new notification-pane% [id 'notifications])
      #:child #:accessor #:mutator)
    
    ; Constructor --------------------------------
    
    (super-new)
    
    ; Methods ------------------------------------
    
    ; -> void
    (define/public #:callback (on-notify num sticky?)
      (for ([i (in-range 1 (add1 num))])
        (notifications-add! (xml ,(if sticky? "Sticky notification " "Notification ") ,i) sticky?)))
    
    ; seed -> xml
    (define/augment (render seed)
      (xml (h1 "Notification page " ,page-number)
           (p (a (@ [href ,(controller-url notification1)]) "Visit page 1") " "
              (a (@ [href ,(controller-url notification2)]) "Visit page 2"))
           ,(send notification-pane render seed)
           (ul ,@(for/list ([i (in-range 1 4)])
                   (xml (li (a (@ [id ,(format "normal-~a-full" i)] [href ,(embed seed (callback on-notify i #f))])
                               "Create " ,i ,(if (= i 1) " notification" " notifications") " (full page refresh)"))))
               ,@(for/list ([i (in-range 1 4)])
                   (xml (li (a (@ [id ,(format "sticky-~a-full" i)] [href ,(embed seed (callback on-notify i #t))])
                               "Create " ,i ,(if (= i 1) " sticky notification" " sticky notifications") " (full page refresh)"))))
               ,@(for/list ([i (in-range 1 4)])
                   (xml (li (a (@ [id ,(format "normal-~a-ajax" i)] [href "javascript:void(0)"] [onclick ,(embed/ajax seed (callback on-notify i #f))])
                               "Create " ,i ,(if (= i 1) " notification" " notifications") " (AJAX refresh)"))))
               ,@(for/list ([i (in-range 1 4)])
                   (xml (li (a (@ [id ,(format "sticky-~a-ajax" i)] [href "javascript:void(0)"] [onclick ,(embed/ajax seed (callback on-notify i #t))])
                               "Create " ,i ,(if (= i 1) " sticky notification" " sticky notifications") " (AJAX refresh)")))))))))

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
