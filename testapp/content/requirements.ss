#lang scheme/base

(require "../content-base.ss")

; Controllers ------------------------------------

; request -> response
(define-controller (requirements)
  (send requirements-page respond))

; Components -------------------------------------

(define requirements-page
  (singleton/cells html-page% ()
    
    ; Fields -------------------------------------
    
    ; (cell (listof html-component<%>))
    (cell children null #:children #:accessor #:mutator)
    
    ; Constructor --------------------------------
    
    (super-new [title "Requirements"])
    
    ; Methods ------------------------------------
    
    ; -> (listof (U xml (seed -> xml)))
    (define/augment (get-html-requirements)
      (list* page-html-requirement
             (inner null get-html-requirements)))
    
    ; -> (listof (U xml (seed -> xml)))
    (define/augment (get-js-requirements)
      (list* page-js-requirement
             (inner null get-js-requirements)))
    
    ; seed -> xml
    (define/augment (render seed)
      (xml (p (a (@ [id 'ajax] [onclick ,(embed/ajax seed (callback on-refresh))])
                 "Add requirement (AJAX)")
              " "
              (a (@ [id 'full] [href ,(embed seed (callback on-refresh))])
                 "Add requirement (Full)"))
           ,@(for/list ([child (in-list (get-children))])
               (send child render seed))))
    
    (define/public #:callback (on-refresh)
      (when (null? (get-children))
        (set-children! (list requirements-child))))))

(define requirements-child
  (singleton/cells html-component% ()
    
    ; Methods ------------------------------------
    
    ; -> (listof (U xml (seed -> xml)))
    (define/augment (get-html-requirements)
      (list* child-html-requirement
             (inner null get-html-requirements)))
    
    ; -> (listof (U js (seed -> js)))
    (define/augment (get-js-requirements)
      (list* child-js-requirement
             (inner null get-js-requirements)))
    
    (define/override (render seed)
      (xml (p "Requirements loaded")))))

; HTML and JS requirements -----------------------

; xml
(define page-html-requirement
  (xml (script (@ [type "text/javascript"])
               (!raw "\n// <![CDATA[\n")
               (!raw ,(js (!dot Smoke (log "page html"))
                          (= (!dot window htmlReqsLoaded)
                             (? (=== (!dot window htmlReqsLoaded) undefined)
                                1
                                (+ (!dot window htmlReqsLoaded) 1)))))
               (!raw "\n// ]]>\n"))))

; js
(define page-js-requirement
  (js (!dot Smoke (log "page js"))
      (= (!dot window jsReqsLoaded)
         (? (=== (!dot window jsReqsLoaded) undefined)
            1
            (+ (!dot window jsReqsLoaded) 1)))))

; xml
(define child-html-requirement
  (xml (script (@ [type "text/javascript"])
               (!raw "\n// <![CDATA[\n")
               (!raw ,(js (!dot Smoke (log "child html"))
                          (= (!dot window htmlReqsLoaded)
                             (? (=== (!dot window htmlReqsLoaded) undefined)
                                1
                                (+ (!dot window htmlReqsLoaded) 1)))))
               (!raw "\n// ]]>\n"))))

; js
(define child-js-requirement
  (js (!dot Smoke (log "child js"))
      (= (!dot window jsReqsLoaded)
         (? (=== (!dot window jsReqsLoaded) undefined)
            1
            (+ (!dot window jsReqsLoaded) 1)))))

; Provide statements -----------------------------

(provide requirements-page)
