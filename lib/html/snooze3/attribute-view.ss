#lang scheme/base

(require (planet untyped/snooze:3)
         (planet untyped/unlib:3/symbol)
         "../../../lib-base.ss"
         "../html-element.ss"
         "../labelled-element.ss"
         "check-label.ss"
         "view-interface.ss")

; Interfaces -------------------------------------

(define attribute-view<%>
  (interface (view<%> labelled-element<%> check-label<%>)
    get-attributes ; -> (listof attribute)
    destructure!)) ; snooze-struct -> void

; Mixins -----------------------------------------

(define attribute-view-mixin
  (mixin/cells (html-element<%> labelled-element<%> check-label<%> view<%>) (attribute-view<%>)
    
    (inherit core-html-attributes
             get-component-id
             get-id
             set-id!
             render-label
             render-check-label
             set-label!)
    
    ; Fields -------------------------------------
    
    (super-new)
    
    ; (cell (listof attribute))
    (init-cell attributes null #:accessor)
    
    ; (cell any)
    (init-cell value #f #:accessor #:mutator)
    
    (init [id    (if (pair? attributes)
                     (let ([attr (car attributes)])
                       (symbol-append (entity-name (attribute-entity attr)) '- (attribute-name attr)))
                     (get-component-id))]
          [label (if (pair? attributes)
                     (let ([attr (car attributes)])
                       (xml-quote (string-titlecase (attribute-pretty-name attr))))
                     (xml-quote id))])
    
    (set-id!   id)
    (set-label! label)
    
    ; Methods ------------------------------------
    
    ; check-result -> boolean
    (define/override (report-result? result)
      (or (memq this (check-result-annotation result ann:form-elements))
          (ormap (cut check-result-has-attribute? result <>)
                 (get-attributes))))
    
    ; -> symbol
    (define/public (get-wrapper-id)
      (symbol-append (get-id) '-wrapper))
    
    ; seed -> xml
    (define/override (render seed)
      (xml (span (@ ,(core-html-attributes seed))
                 ,(let ([val (get-value)])
                    (if (snooze-struct? val)
                        (xml-quote (format-snooze-struct val))
                        (xml-quote val)))
                 ,(render-check-label seed))))
    
    ; snooze-struct -> snooze-struct
    (define/public (destructure! struct)
      (match (get-attributes)
        [(list-rest (? attribute? attr) _)
         (set-value! (snooze-struct-ref struct attr))]
        [attrs (raise-type-error 'attribute-view.destructure! "(list attribute attribute ...)" attrs)]))))

; Classes ----------------------------------------

(define complete-attribute-view-mixin
  (compose attribute-view-mixin check-label-mixin labelled-element-mixin simple-view-mixin))

(define attribute-view%
  (complete-attribute-view-mixin html-element%))

; Procedures -------------------------------------

; attribute -> attribute-view<%>
(define (default-attribute-view attr)
  (new attribute-view% [attributes (list attr)]))

; Provide statements -----------------------------

(provide attribute-view<%>
         attribute-view-mixin
         attribute-view%)

(provide/contract
 [default-attribute-view (-> attribute? (is-a?/c attribute-view<%>))])