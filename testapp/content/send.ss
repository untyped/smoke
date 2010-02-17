#lang scheme/base

(require "../content-base.ss")

(require "../../core/send.ss")

(define-page send-page html-page% ()

  (inherit respond)
  
  ; Fields ---------------------------------------
  
  (init-cell counter 0
    #:accessor #:mutator)
  
  (init-cell k-url #f
    #:accessor #:mutator)
  
  ; Constructor ----------------------------------
  
  (super-new)
  
  ; Methods --------------------------------------
  
  (define/augment (render seed)
    (xml (p "Counter " ,(get-counter))
         ,(if (get-k-url)
              (xml (p (a (@ [href ,(get-k-url)])
                         "continue")))
              (xml (p (a (@ [href ,(embed seed (callback on-send-suspend))])
                         "send/suspend"))
                   (p (a (@ [href ,(embed seed (callback on-send-suspend-dispatch))])
                         "send/suspend/dispatch"))))))
  
  (define/public #:callback (on-send-suspend)
    (set-counter! (add1 (get-counter)))
    (send/suspend
     (lambda (k-url)
       (set-k-url! k-url)
       (respond)))
    (set-k-url! #f))
  
  (define/public #:callback (on-send-suspend-dispatch)
    (set-counter! (add1 (get-counter)))
    (send/suspend/dispatch
     (lambda (embed-url)
       (set-k-url!
        (embed-url
         (lambda ()
           (set-k-url! #f))))
       (respond)))))
