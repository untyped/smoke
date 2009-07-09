#lang scheme/base

(require srfi/13
         "../content-base.ss")

; Controllers ------------------------------------

; request -> response
(define-controller autocomplete
  init-smoke-pipeline
  (lambda ()
    (send autocomplete-page respond)))

; Pages ------------------------------------------

(define autocomplete-page
  (singleton/cells (refreshable-mixin html-page%) ()
    
    (inherit refresh!)
    
    ; Fields -------------------------------------
    
    (field autocomplete
      (new autocomplete-field%
           [id        'autocomplete]
           [options   '("Alpha" "Alpaca" "Alpine" "Alpacino" "Other" "Otter")]
           [on-change (callback on-autocomplete-change)])
      #:accessor #:mutator #:child)
    
    (field button
      (new button%
           [id 'button]
           [on-click (callback on-complete-refresh)]
           [label "Refresh the page via AJAX"])
      #:accessor #:mutator #:child)
    
    (cell change-count 0 #:accessor #:mutator)
    
    ; Constructor --------------------------------
    
    (super-new [title "Autocomplete"])
    
    ; Methods ------------------------------------
    
    ; seed -> xml
    (define/augride (render seed)
      (xml (p "There is currently no way of automating these tests with Delirium. "
              "Make sure:")
           (ul (li "typing \"Alp\" brings up 4 options;")
               (li "pressing up/down selects the options;")
               (li "pressing \"Enter\" on an option selects it;")
               (li "pressing \"Esc\" on an option does not select it;")
               (li "clicking on an option selects it;")
               (li "clicking elsewhere does not select an option;")
               (li "moving the mouse over and out of the popup does not hide it;")
               (li "refreshing the page using AJAX does not break any of the above."))
           (p ,(send autocomplete render seed) " "
              ,(send button render seed))
           (p "Change count: " ,(get-change-count) ", last updated value: " ,(send autocomplete get-value))))
    
    ; -> anc
    (define/public #:callback (on-complete-refresh)
      (refresh!))
    
    (define/public #:callback (on-autocomplete-change)
      (set-change-count! (add1 (get-change-count))))))
