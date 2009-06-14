#lang scheme/base

(require srfi/26/cut
         "../../lib-base.ss"
         "browser-util.ss"
         "html-element.ss"
         "labelled-element.ss")

; Interfaces -------------------------------------

(define tab<%>
  (interface (labelled-element<%>)
    get-inline?             ; -> boolean
    get-content-visible?    ; -> boolean
    set-content-visible?!)) ; boolean -> void

; Classes -----------------------------------------

(define tab%
  (class/cells (labelled-element-mixin html-element%) (tab<%>)
    
    (inherit core-html-attributes)
    
    ; Fields -------------------------------------
    
    ; boolean
    (init-field inline? #:accessor)
    
    ; html-component<%>
    (init-field content #:child #:accessor #:mutator)
    
    ; Constructor --------------------------------
    
    (init [classes null])
    
    (super-new [classes `(smoke-tab ,@classes)])
    
    (send content set-visible?! inline?)
    
    ; Methods ------------------------------------
    
    ; -> boolean
    (define/public (get-content-visible?)
      (send content get-visible?))
    
    ; boolean -> void
    (define/public (set-content-visible?! val)
      (send content set-visible?! val))
    
    ; seed -> xml
    (define/override (render seed)
      (xml (div (@ ,@(core-html-attributes seed))
                ,(send (get-content) render seed))))))

(define tab-pane%
  (class/cells html-element% ()
    
    (inherit get-id
             core-html-attributes)
    
    ; Fields -------------------------------------
    
    ; (cell (U (listof tab%) (-> (listof tab%)))
    (init-cell tabs #:mutator)
    
    ; (cell (U tab% #f))
    (init-cell current-tab
      (if (procedure? tabs)
          (car (tabs))
          (car tabs))
      #:accessor #:mutator)
    
    ; Constructor --------------------------------
    
    ; (listof symbol)
    (init [classes null])
    
    (super-new [classes `(smoke-tab-pane ,@classes)])
    
    (send current-tab set-content-visible?! #t)
    
    ; Methods ------------------------------------
    
    ; -> (listof tab<%>)
    (define/override (get-child-components)
      (get-tabs))
    
    ; -> boolean
    (define/override (dirty?)
      (web-cell-changed? tabs-cell))
    
    ; -> (listof tab%)
    (define/public (get-tabs)
      (define tabs (web-cell-ref tabs-cell))
      (if (procedure? tabs)
          (tabs)
          tabs))
    
    ; symbol -> (U tab% #f)
    (define/public (get-tab id)
      (ormap (lambda (tab)
               (and (eq? (send tab get-id) id) tab))
             (get-tabs)))
    
    ; -> (listof (U xml (seed -> xml)))
    (define/augment (get-html-requirements)
      (list* jquery-ui-script
             jquery-ui-styles
             (inner null get-html-requirements)))
    
    ; seed -> xml
    (define/override (render seed)
      ; (listof tab%)
      (define tabs
        (get-tabs))
      ; tab%
      (define current-tab
        (get-current-tab))
      ; (listof xml)
      ; (listof xml)
      (define-values (labels-xml tabs-xml)
        (for/fold ([label-accum null]
                   [tab-accum null])
                  ([tab (in-list (reverse tabs))])
                  (values
                   ; Labels:
                   (cons (xml (li (a (@ [href ,(format "#~a" (send tab get-id))])
                                     ,(send tab render-label seed))))
                         label-accum)
                   ; Tabs:
                   (cons (send tab render seed) tab-accum))))
      (xml (div (@ ,@(core-html-attributes seed))
                ,(opt-xml (pair? tabs)
                   (ul ,@labels-xml)
                   ,@tabs-xml))))
    
    ; seed -> js
    (define/augment (get-on-attach seed)
      (define current-tab (get-current-tab))
      
      (define-values (on-tab-show-clauses current-tab-pos)
        (for/fold ([on-tab-show-accum     null]
                   [current-tab-pos-accum 0])
                  ([tab   (in-list (get-tabs))]
                   [index (in-naturals)])
                  (values (if (send tab get-inline?)
                              on-tab-show-accum
                              (cons (js (if (&& (!= panelId currentTabId)
                                                (== panelId ,(send tab get-id)))
                                            (!block (!dot console (log (+ "Loading " panelId)))
                                                    (= currentTabId panelId)
                                                    ,(embed/ajax seed (callback on-load (send tab get-id))))))
                                    on-tab-show-accum))
                          (if (eq? tab current-tab)
                              index
                              current-tab-pos-accum))))
      
      (js (!block (var [currentTabId  ,(send (get-current-tab) get-id)]
                       [currentTabPos ,current-tab-pos]
                       [onTabShow     (function (evt ui)
                                        (var [panelId (!dot ui panel id)])
                                        (!dot Smoke (log evt ui))
                                        ,@on-tab-show-clauses)])
                  (!dot ($ ,(format "#~a" (get-id)))
                        (tabs (!object [selected currentTabPos]
                                       [show     onTabShow])))
                  ,(inner (js) get-on-attach seed))))
    
    ; seed -> js
    (define/augment (get-on-detach seed)
      (js ,(inner (js) get-on-detach seed)
          (!dot ($ ,(format "#~a" (get-id))) (tabs "destroy"))))
    
    ; integer -> void
    (define/public #:callback (on-load id)
      (let ([current-tab (get-tab id)])
        (set-current-tab! current-tab)
        (for ([tab (in-list (get-tabs))])
          (let* ([tab-id   (send tab get-id)]
                 [current? (eq? tab current-tab)]
                 [inline?  (send tab get-inline?)]
                 [loaded?  (eq? tab-id id)])
            (send tab set-content-visible?! (or current? inline? loaded?))))))))

; Provide statements -----------------------------

(provide tab<%>
         tab%
         tab-pane%)
