#lang scheme

(require "../../../lib-base.ss"
         "../../component.ss"
         "../html-element.ss")

(define scroll-report%
  (class/cells html-element% ()
    
    (inherit core-html-attributes
             get-id)
    
    ; Fields -------------------------------------
    
    ; (cell integer)
    (init-cell total 1000 #:accessor #:mutator)
    
    ; (cell integer)
    (init-cell count 25 #:accessor #:mutator)
    
    ; Constructor --------------------------------
    
    ; (listof (U symbol string))
    (init [classes null])
    
    (super-new [classes (cons 'smoke-scroll-report classes)])
    
    ; Methods ------------------------------------
    
    ; -> (listof (U xml (seed -> xml)))
    (define/augment (get-html-requirements)
      (list* scroll-report-script
             scroll-report-styles
             (inner null get-html-requirements)))
    
    ; seed -> xml
    (define/override (render seed)
      (define total (get-total))
      (define count (get-count))
      (xml (div (@ ,@(core-html-attributes seed))
                (div (@ [class "smoke-scroll-report-canvas"])))))
    
    ; seed -> js
    (define/augride (get-on-attach seed)
      (js (!dot ($ ,(format "#~a" (get-id)))
                (initScrollReport 1000 250))))))

; Helpers ----------------------------------------

; xml
(define scroll-report-script
  (xml (script (@ [type "text/javascript"]
                  [src "/scripts/smoke/scroll-report.js"]))))

; xml
(define scroll-report-styles
  (xml (style (@ [type "text/css"])
              (!raw #<<ENDCSS
.smoke-scroll-report {
    border:     1px solid black;
    overflow-y: scroll;
}

.smoke-scroll-report-canvas {
    border: 1px solid green;
    background: #fff;
}

.smoke-scroll-report-canvas tr.even { background: #ffffff; }
.smoke-scroll-report-canvas tr.odd  { background: #ffffff; }

ENDCSS
                    ))))

; Provide statements -----------------------------

(provide scroll-report%)
