#lang scheme/base

(require srfi/26/cut
         "../../lib-base.ss"
         "browser-util.ss"
         "html-element.ss"
         "labelled-element.ss")

; Interfaces -------------------------------------

(define tab<%>
  (interface (labelled-element<%>)
    ; -> boolean
    is-inline?))

; Mixins -----------------------------------------

(define tab-mixin
  (compose
   (mixin/cells (html-element<%>) (tab<%>)
     
     ; Fields -------------------------------------
     
     ; (field boolean)
     (init-field inline? #f #:accessor is-inline?)
     
     ; Constructor --------------------------------
     
     ; (listof symbol)
     (init [classes null])
     
     (super-new [classes `(smoke-tab ,@classes)]))
   labelled-element-mixin))

; Classes ----------------------------------------

(define tab%
  (class/cells (tab-mixin html-element%) ()
    
    (inherit core-html-attributes)
    
    ; Fields -------------------------------------
    
    ; html-component<%>
    (init-field content #:child #:accessor #:mutator)
    
    ; Methods ------------------------------------
    
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
    
    ; Methods ------------------------------------
    
    ; -> (listof tab<%>)
    (define/override (get-child-components)
      (let ([current-tab (get-current-tab)])
        (for/fold ([accum null])
                  ([tab   (in-list (reverse (get-tabs)))])
                  (let ([current? (eq? tab current-tab)]
                        [inline?  (send tab is-inline?)])
                    (if (or current? inline?)
                        (cons tab accum)
                        accum)))))
    
    ; -> (listof tab%)
    (define/public (get-tabs)
      (define tabs (web-cell-ref tabs-cell))
      (if (procedure? tabs)
          (tabs)
          tabs))
    
    ; symbol -> (U tab% #f)
    (define/public (get-tab id)
      (ormap (lambda (tab)
               (and (eq? (send tab get-component-id) id) tab))
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
                  (let ([current? (eq? tab current-tab)]
                        [inline?  (send tab is-inline?)])
                    (values (let ([href (if (or current? inline?)
                                            (format "#~a" (send tab get-id))
                                            (embed seed (callback on-load (send tab get-component-id))))])
                              (cons (xml (li (a (@ [href ,href])
                                                ,(send tab render-label seed))))
                                    label-accum))
                            (if (or current? inline?)
                                (cons (send tab render seed) tab-accum)
                                tab-accum)))))
      (xml (div (@ ,@(core-html-attributes seed))
                           ,(opt-xml (pair? tabs)
                              (ul ,@labels-xml)
                              ,@tabs-xml))))
    
    ; seed -> js
    (define/augment (get-on-attach seed)
      (js (!dot ($ ,(format "#~a" (get-id))) (tabs))
          ,(inner (js) get-on-attach seed)))
    
    ; seed -> js
    (define/augment (get-on-detach seed)
      (js ,(inner (js) get-on-detach seed)
          (!dot ($ ,(format "#~a" (get-id))) (tabs "destroy"))))
    
    ; integer -> void
    (define/public #:callback/return (on-load cid)
      (send/suspend/dispatch
       (lambda (embed-url)
         (let ([seed (make-seed (current-page) embed-url)])
           (make-html-response (send (get-tab cid) render seed))))))))

; Provide statements -----------------------------

(provide tab<%>
         tab-mixin
         tab%
         tab-pane%)
