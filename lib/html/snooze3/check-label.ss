#lang scheme/base

(require (planet untyped/snooze:3)
         (planet untyped/unlib:3/symbol)
         "../../../lib-base.ss"
         "../browser-util.ss"
         "../html-element.ss"
         "util.ss")

; Interfaces -------------------------------------

(define check-label<%>
  (interface ()
    report-result?                  ; check-result -> void
    set-results!                    ; (listof check-result) -> void
    render-check-label))            ; seed [boolean] -> xml

; Mixins -----------------------------------------

(define check-label-mixin
  (mixin/cells (html-element<%>) (check-label<%>)
    
    (inherit get-id
             core-html-attributes)
    
    ; Fields -------------------------------------
    
    ; (cell (listof check-result))
    (init-cell [results null] #:accessor #:mutator)
    
    ; Methods ------------------------------------
    
    ; check-result -> boolean
    (define/public (report-result? result)
      (error "report-result? must be overridden"))
    
    ; -> (listof (U xml (seed -> xml)))
    (define/augment (get-html-requirements)
      (list* tooltip-script
             snooze-styles
             (inner null get-html-requirements)))
    
    ; seed (listof check-result) [boolean] -> xml
    (define/public (render-check-results seed results [tooltip? #t])
      (xml (ul (@ [class ,(if tooltip? 
                              "check-results tooltip"
                              "check-results")])
               ,@(for/list ([result results])
                   (define class (check-result->class result))
                   (xml (li (@ [class ,class])
                            ,(check-result-message result)))))))
    
    ; seed [boolean] -> xml
    (define/public (render-check-label seed [tooltip? #t])
      ; (listof check-result)
      (define reportable-results
        (filter (cut report-result? <>) 
                (get-results)))
      ; (U 'check-success 'check-warning 'check-failure 'check-exn)
      (define class
        (check-results->class reportable-results))
      ; xml
      (xml (span (@ [class ,(if tooltip? "check-label tooltip-anchor" "check-label")])
                 ,(opt-xml (not (eq? class 'check-success))
                    ,(check-result-icon class)
                    ,(render-check-results seed reportable-results tooltip?)))))))

; Classes ----------------------------------------

(define check-label%
  (class/cells (check-label-mixin html-element%) ()
    
    (inherit get-id
             core-html-attributes
             render-check-label)
    
    ; Fields -------------------------------------
    
    ; (check-result -> boolean)
    (init-field predicate (lambda (result) #t)
      #:accessor)
    
    ; boolean
    (init-field tooltip? #t
      #:accessor render-as-tooltip?)
    
    ; Methods ------------------------------------
    
    ; check-result -> boolean
    (define/override (report-result? result)
      ((get-predicate) result))
    
    ; seed -> xml
    (define/override (render seed)
      (xml (span (@ ,(core-html-attributes seed))
                 ,(render-check-label seed (render-as-tooltip?)))))))

; Provide statements -----------------------------

(provide (all-from-out "util.ss")
         check-label-mixin
         check-label<%>
         check-label%)
