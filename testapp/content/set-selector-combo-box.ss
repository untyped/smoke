#lang scheme/base

(require srfi/13
         "../content-base.ss")

; Controllers ------------------------------------

; controller
(define-controller (set-selector-combo-box request)
  (send set-selector-combo-box-page respond))

; Pages ------------------------------------------

(define set-selector-combo-box-page
  (singleton/cells (refreshable-mixin html-page%) ()
    
    (inherit refresh!)
    
    ; Fields -------------------------------------
    
    (field [set-selector-combo-box
            (new set-selector-combo-box-field%
                 [id        'set-selector-combo-box]
                 [available-items `(,(alpha    . "Alpha")
                                    ,(alpaca   . "Alpaca")
                                    ,(alpine   . "Alpine")
                                    ,(alpacino . "Alpacino")
                                    ,(other    . "Other")
                                    ,(otter    . "Otter"))]
                 [on-change (callback on-set-selector-combo-box-change)])]
      #:accessor #:mutator #:child)
    
    (field [button
            (new button%
                 [id       'button]
                 [on-click (callback on-complete-refresh)]
                 [label    "Refresh the page via AJAX"])]
      #:accessor #:mutator #:child)
    
    (cell [change-count 0]
      #:accessor #:mutator)
    
    ; Constructor --------------------------------
    
    (super-new [title "Set-selector-combo-box"])
    
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
           (p ,(send set-selector-combo-box render seed) " "
              ,(send button render seed))
           (p "Change count: " ,(get-change-count) ", last updated value: " ,(send set-selector-combo-box get-value))))
    
    ; -> anc
    (define/public #:callback (on-complete-refresh)
      (refresh!))
    
    (define/public #:callback (on-set-selector-combo-box-change)
      (set-change-count! (add1 (get-change-count))))))
