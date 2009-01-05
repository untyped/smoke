#lang scheme/base

(require srfi/26/cut
         "../../lib-base.ss"
         "html-element.ss")

; Components -------------------------------------

(define tab%
  (class/cells html-element% ()
    
    (inherit core-html-attributes)
    
    ; Fields -------------------------------------
    
    ; (cell xml)
    (init-cell label
      #:accessor #:mutator)
    
    ; (cell html-component<%>)
    (init-cell content
      #:child #:accessor #:mutator)
    
    ; (listof symbol)
    (init [classes null])
    
    ; Constructor --------------------------------
    
    (super-new [classes (cons 'smoke-tab classes)])
    
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
    (init-cell tabs
      #:mutator)
    
    ; (cell (U tab% #f))
    (init-cell [current-tab (if (procedure? tabs)
                                (car (tabs))
                                (car tabs))]
      #:child #:accessor #:mutator)
    
    ; (listof symbol)
    (init [classes null])
    
    ; Constructor --------------------------------
    
    (super-new [classes (cons 'smoke-tab-pane classes)])
    
    ; Methods ------------------------------------
    
    ; -> (listof tab%)
    (define/public (get-tabs)
      (define tabs 
        (web-cell-ref tabs-cell))
      (if (procedure? tabs)
          (tabs)
          tabs))
    
    ; symbol -> (U tab% #f)
    (define/public (get-tab id)
      (ormap (lambda (tab)
               (and (eq? (send tab get-component-id) id) tab))
             (get-tabs)))
    
    ; seed -> xml
    (define/override (render seed)
      ; (listof tab%)
      (define tabs
        (get-tabs))
      ; tab%
      (define current-tab
        (get-current-tab))
      ; xml
      (xml (div (@ ,@(core-html-attributes seed))
                ,(opt-xml (not (null? tabs))
                   (ul (@ [class 'labels])
                       ,@(map (lambda (tab)
                                ; symbol
                                (define id
                                  (send tab get-component-id))
                                ; boolean
                                (define current?
                                  (eq? tab current-tab))
                                ; (U symbol #f)
                                (define class
                                  (and current? 'current))
                                ; (U callback #f)
                                (define onclick
                                  (and (not current?)
                                       (embed/ajax seed (callback on-select id))))
                                ; xml
                                (xml (li (@ ,(opt-xml-attr class))
                                         (a (@ [class "left"] ,(opt-xml-attr onclick))
                                            (span (@ [class "right"])
                                                  ,(send tab get-label))))))
                              (get-tabs)))
                   (div (@ [class 'current-tab])
                        ,(send current-tab render seed))))))
    
    ; integer -> void
    (define/public #:callback (on-select id)
      (set-current-tab! (get-tab id)))))

; Provide statements -----------------------------

(provide tab%
         tab-pane%)
