#lang scheme/base

(require "../content-base.ss")

(define-page k-page (k-html-page-mixin html-page%) ()

  (inherit respond)
  
  ; Fields ---------------------------------------
  
  (init-cell counter 0
    #:accessor #:mutator)
  
  ; Constructor ----------------------------------
  
  (super-new)
  
  ; Methods --------------------------------------
  
  ; -> void
  (define/override (dispatch)
    (let loop ()
      (set-counter! (+ (get-counter) (respond)))
      (loop)))
  
  (define/augment (render seed)
    (xml (p "site "    (span (@ [id "current-site"]) ,(format "~a" (current-site))))
         (p "page "    (span (@ [id "current-page"]) ,(format "~a" (current-page))))
         (p "counter " (span (@ [id "counter"]) ,(get-counter)))
         (p (a (@ [id "increment"] [href ,(embed seed (callback on-increment))]) "increment by callback"))
         (p (a (@ [id "decrement"] [href ,(embed seed (callback on-decrement))]) "decrement by callback"))))
  
  (define/public #:callback* (on-increment)
    1)
  
  (define/public #:callback* (on-decrement)
    -1))
