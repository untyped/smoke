#lang scheme

(require "../../test-base.ss"
         "../../smoke.ss")

; Tests ------------------------------------------

(define html-element-tests
  (test-suite "html-element.ss"

    '#:before
    (lambda ()
      (set! on-submit-arg 'undefined)
      (set! on-submit-opt-arg 'undefined)
      (set! on-submit-rest-arg 'undefined))
    
    (test-case "test page loaded"
      (open/page test-page)
      (check-equal? (title-ref) "HTML element tests")
      (check-equal? on-submit-arg 'undefined)
      (check-equal? on-submit-opt-arg 'undefined)
      (check-equal? on-submit-rest-arg 'undefined))
    
    (test-case "thunk button"
      (click/wait (node/id 'thunk-button))
      (check-equal? (title-ref) "HTML element tests")
      (check-equal? on-submit-arg 123)
      (check-equal? on-submit-opt-arg #f)
      (check-equal? on-submit-rest-arg null))
    
    (test-case "callback button"
      (click/wait (node/id 'callback-button))
      (check-equal? (title-ref) "HTML element tests")
      (check-equal? on-submit-arg 321)
      (check-equal? on-submit-opt-arg "str")
      (check-equal? on-submit-rest-arg  '((1 2 3) #hasheq((key . "val")))))
    
    #;(test-case "combo-box"
      (select (node/id 'combo-box) "a")
      (read-line)
      (check-equal? (title-ref) "HTML element tests")
      (check-equal? on-submit-arg 1000)
      (set! on-submit-arg #f)
      (select (node/id 'combo-box) "b")
      (check-equal? (title-ref) "HTML element tests")
      (check-equal? on-submit-arg #f)
      (select (node/id 'combo-box) "c")
      (check-equal? (title-ref) "HTML element tests")
      (check-equal? on-submit-arg 1000))))
    
; Helpers ----------------------------------------

; json-serializable
; json-serializable
; (listof json-serializable)
(define on-submit-arg 'undefined)
(define on-submit-opt-arg 'undefined)
(define on-submit-rest-arg 'undefined)

(define test-page
  (singleton/cells html-page% ()
    
    ; Constructor --------------------------------
    
    (super-new [id 'page] [title "HTML element tests"] [component-id 'page])
    
    ; Fields -------------------------------------
    
    ; button%
    (field thunk-button
      (new button%
           [id       'thunk-button]
           [label    "Thunk button"]
           [on-click (cut on-submit 123)])
      #:child #:accessor)
    
    ; button%
    (field callback-button
      (new button%
           [id       'callback-button]
           [label    "Callback button"]
           [on-click (callback on-submit 321 "str" '(1 2 3) #hasheq((key . "val")))])
      #:child #:accessor)
    
    ; combo-box~%
    (field combo-box
      (new combo-box% 
           [id        'combo-box]
           [value     'a]
           [options   '((a . "a") (b . "b") (c . "c"))]
           [on-change (callback on-submit 1000)])
      #:child #:accessor)
    
    ; Methods ------------------------------------
    
    ; json-serializable -> (U js void)
    (define/override #:callback (on-submit arg [opt-arg #f] . rest-arg)
      (set! on-submit-arg arg)
      (set! on-submit-opt-arg opt-arg)
      (set! on-submit-rest-arg rest-arg))))

; Provide statements -----------------------------

(provide html-element-tests)
