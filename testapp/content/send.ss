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
    (xml (p "site "    (span (@ [id "current-site"]) ,(format "~a" (current-site))))
         (p "page "    (span (@ [id "current-page"]) ,(format "~a" (current-page))))
         (p "counter " (span (@ [id "counter"]) ,(get-counter)))
         (p "k-url "   (span (@ [id "k-url"]) ,(get-k-url)))
         (p (a (@ [id   "increment"]
                  [href ,(embed seed (callback on-increment))])
               "increment by callback"))
         (p (a (@ [id   "send-suspend"]
                  [href ,(embed seed (callback on-send-suspend))])
               "enter send/suspend"))
         (p (a (@ [id   "send-suspend-dispatch"]
                  [href ,(embed seed (callback on-send-suspend-dispatch))])
               "enter send/suspend/dispatch"))
         ,(opt-xml (get-k-url)
            (p (a (@ [id   "continue"]
                     [href ,(get-k-url)])
                  "continue")))))
  
  (define/public #:callback (on-increment)
    (set-counter! (add1 (get-counter))))
  
  (define/public #:callback (on-send-suspend)
    (let loop ()
      (set-counter! (add1 (get-counter)))
      (send/suspend
       (lambda (k-url)
         (set-k-url! k-url)
         (respond)))
      (loop)))
  
  (define/public #:callback (on-send-suspend-dispatch)
    (let loop ()
      (set-counter! (add1 (get-counter)))
      (send/suspend/dispatch
       (lambda (embed-url)
         (set-k-url!
          (embed-url
           (lambda ()
             (loop))))
         (respond))))))
