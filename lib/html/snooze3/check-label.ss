#lang scheme/base

(require (planet untyped/snooze:3)
         (planet untyped/unlib:3/symbol)
         "../../../lib-base.ss"
         "../browser-util.ss"
         "../html-element.ss"
         "checkable.ss"
         "editor-interface.ss"
         "util.ss")

; Interfaces -------------------------------------

(define check-label<%>
  (interface (checkable<%>)
    report-result?       ; check-result -> void
    render-check-label)) ; seed [boolean] -> xml

; Mixins -----------------------------------------

(define check-label-mixin
  (mixin/cells (html-element<%> checkable<%>) (check-label<%>)
    
    (inherit get-id
             core-html-attributes)
    
    ; Fields -------------------------------------
    
    ; (cell (listof check-result))
    (init-cell check-results null #:accessor)
    
    ; Methods ------------------------------------
    
    ; check-result -> boolean
    (define/public (report-result? result)
      (and (memq this (check-result-annotation result ann:form-elements)) #t))
    
    ; (listof check-result) -> void
    (define/override (set-check-results! results)
      (super set-check-results! results)
      (web-cell-set! check-results-cell results))
    
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
                (get-check-results)))
      ; (U 'check-success 'check-warning 'check-failure 'check-exn)
      (define class (check-results->class reportable-results))
      ; xml
      (xml (span (@ [class ,(if tooltip? "check-label tooltip-anchor" "check-label")])
                 ,(opt-xml (not (eq? class 'check-success))
                    ,(check-result-icon class)
                    ,(render-check-results seed reportable-results tooltip?)))))))

; Classes ----------------------------------------

(define check-label%
  (class/cells (check-label-mixin simple-editor%) ()
    
    (inherit get-id
             core-html-attributes
             render-check-label)
    
    ; Fields -------------------------------------
    
    ; (cell (check-result -> boolean))
    (init-cell predicate
      (lambda (result) #t)
      #:accessor #:mutator)
    
    (init-field tooltip? #t #:accessor #:mutator)
    
    ; Methods ------------------------------------
    
    ; check-result -> boolean
    (define/override (report-result? result)
      ((get-predicate) result))
    
    ; seed -> xml
    (define/override (render seed)
      (xml (span (@ ,(core-html-attributes seed))
                 ,(render-check-label seed (get-tooltip?)))))))

; Provide statements -----------------------------

(provide (all-from-out "util.ss")
         check-label-mixin
         check-label<%>
         check-label%)
