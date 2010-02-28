#lang scheme

(require (only-in (planet schematics/schemeunit:3/text-ui) display-exn)
         "../../lib-base.ss"
         "html-page.ss")

; Interfaces -------------------------------------

(define k-html-page<%>
  (interface (html-page<%>)))

; Mixins -----------------------------------------

(define-mixin k-html-page-mixin (html-page<%>) (k-html-page<%>)
  
  (inherit make-response)
  
  ;  [#:forward? boolean] -> any
  (define/override (respond #:forward? [forward? #f])
    (unless (current-request)
      (error "No current HTTP request to respond to."))
    (current-page-set! this)
    (when forward?
      (clear-history!)
      (clear-continuation-table!))
    (send/suspend/dispatch
     (lambda (embed/url)
       (make-response (make-k-seed embed/url))))))

; Provide statements -----------------------------

(provide k-html-page-mixin)
