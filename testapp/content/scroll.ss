#lang scheme/base

(require (planet untyped/unlib:3/string)
         "../content-base.ss")

; Controllers ------------------------------------

; request -> response
#;(define-controller (scroll)
    (send scroll-page respond))

; Components -------------------------------------

#;(define scroll-page
    (singleton/cells html-page% ()
      
      ; Fields -------------------------------------
      
      ; text-area%
      (field report
        (new scroll-report%
             [id    'report]
             [style "width: 100%; height: 600px"])
        #:child #:accessor #:mutator)
      
      ; Constructor --------------------------------
      
      (super-new [component-id 'scroll-page] 
                 [title "Scroll report test"])
      
      ; Methods ------------------------------------
      
      ; seed -> xml
      (define/augment (render seed)
        (printf "Rendering page~n")
        (send report render seed))))
