#lang scheme/base

(require (only-in srfi/13/string string-trim-both)
         "../../lib-base.ss"
         "../../test-base.ss"
         "../page.ss"
         "html-component.ss"
         "html-page.ss")

; Tests ------------------------------------------

(define html-page-script-tests
  (test-suite "html-page.ss : scripts"
    
    (test-case "on-request"
      (open/page n0)
      (check-equal? (title-ref) "Script test")
      (set! request-list null)
      (click/wait (node/link/text "FULL refresh"))
      (check-equal? (title-ref) "Script test")
      (check-equal? request-list (list 'n7 'n6 'n5 'n4 'n3 'n2 'n1 'n0))
      (set! request-list null)
      (click/wait (node/link/text "AJAX refresh"))
      (check-equal? (title-ref) "Script test")
      (check-equal? request-list (list 'n7 'n6 'n5 'n4 'n3 'n2 'n1 'n0))
      (set! request-list null))
    
    (test-case "attach and detach scripts"
      (click/wait (node/link/text "FULL refresh"))
      (check-equal? (title-ref) "Script test")
      (check-equal? (js-ref (js attached)) (list "n3" "n4" "n2" "n6" "n7" "n5" "n1" "n0") "initial attached")
      (check-equal? (js-ref (js detached)) (list) "initial detached")
      (click/wait (node/link/text "AJAX refresh"))
      (check-equal? (js-ref (js attached)) (list "n3" "n4" "n2" "n6" "n7" "n5" "n1" "n0") "AJAX noop attached")
      (check-equal? (js-ref (js detached)) (list) "AJAX noop detached")
      (click/wait (node/link/text "AJAX update"))
      (check-equal? (js-ref (js attached)) (list "n3" "n4" "n2" "n6" "n7" "n5" "n1" "n0" "n3" "n2" "n4" "n7") "AJAX change attached")
      (check-equal? (js-ref (js detached)) (list  "n3" "n4" "n2" "n7") "AJAX change detached"))
    
    (test-case "new init-scripts in AJAX"
      (click/wait (node/link/text "AJAX init script update"))
      (check-equal? (js-ref (js modifiedNodeLoaded)) #t))
    
    ))

; Test classes -----------------------------------

(define node%
  (class/cells html-element% ()
    
    ; -> symbol
    (inherit core-html-attributes
             get-component-id)
    
    ; Fields -------------------------------------
    
    ; (cell (listof html-component<%>))
    (init-cell children null #:children #:accessor #:mutator)
    
    (super-new [style "border: 1px solid #aaa; padding: 2px; margin: 4px"])
    
    ; Methods ------------------------------------
    
    ; request -> void
    (define/augment (on-request request)
      (set! request-list (cons (get-component-id) request-list)))
    
    ; seed -> xml
    (define/override (render seed)
      (xml (div (@ ,@(core-html-attributes seed))
                ,(get-component-id)
                ,(super render seed))))
    
    ; -> (listof (U js (seed -> js)))
    (define/augment (get-js-requirements)
      (list* attach/detach-script
             (inner null get-js-requirements)))
    
    ; seed -> js
    (define/augride (get-on-attach seed)
      (js (attach ,(get-component-id))))
    
    ; seed -> js
    (define/augride (get-on-detach seed)
      (js (detach ,(get-component-id))))))

(define modified-node%
  (class/cells node% ()
    
    ; -> (listof (U js (seed -> js)))
    (define/augment (get-js-requirements)
      (list* attach/detach-script
             modified-node-script
             (inner null get-js-requirements)))))

; Test components --------------------------------

(define n8 (new modified-node% [component-id 'n8]))
(define n7 (new node% [component-id 'n7]))
(define n6 (new node% [component-id 'n6]))
(define n5 (new node% [component-id 'n5] [children (list n6 n7)]))
(define n4 (new node% [component-id 'n4]))
(define n3 (new node% [component-id 'n3]))
(define n2 (new node% [component-id 'n2] [children (list n3 n4)]))
(define n1 (new node% [component-id 'n1] [children (list n2 n5)]))

(define n0
  (singleton/cells (html-page-mixin (page-mixin node%)) ()
    
    (super-new [component-id 'n0]
               [title        "Script test"]
               [children     (list n1)])
    
    ; seed -> xml
    (define/override (render seed)
      (xml ,(super render seed)
           (div (a (@ [href ,(embed seed void)]) "FULL refresh") " "
                (a (@ [href "#"] [onclick ,(embed/ajax seed void)]) "AJAX refresh") " "
                (a (@ [href "#"] [onclick ,(embed/ajax seed (cut update))]) "AJAX update") " "
                (a (@ [href "#"] [onclick ,(embed/ajax seed (cut init-script-update))]) "AJAX init script update"))))
    
    ; seed -> xml
    (define/public (update)
      (send n2 set-children! (list n3))
      (send n7 set-children! (list n4)))
    
    ; seed -> xml
    (define/public (init-script-update)
      (send n1 set-children! (list n8)))))

; Test init scripts ------------------------------

; js
(define attach/detach-script
  (js (var [attached (!array)]
           [rendered (!array)]
           [detached (!array)])
      (function attach (id)
        ((!dot attached push) id))
      (function render (id)
        ((!dot rendered push) id))
      (function detach (id)
        ((!dot detached push) id))))

; js
(define modified-node-script
  (js (= (!dot window modifiedNodeLoaded) true)))

; Actual testable data ---------------------------

(define request-list null)

; Provide statements -----------------------------

(provide html-page-script-tests)

