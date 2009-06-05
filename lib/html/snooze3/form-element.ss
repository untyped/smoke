#lang scheme/base

(require (planet untyped/snooze:3)
         (planet untyped/unlib:3/symbol)
         "../../../lib-base.ss"
         "../autocomplete-field.ss"
         "../check-box.ss"
         "../combo-box.ss"
         "../date-field.ss"
         "../file-field.ss"
         "../form-element.ss"
         "../html-element.ss"
         "../integer-field.ss"
         "../password-field.ss"
         "../radio-button.ss"
         "../regexp-field.ss"
         "../set-selector.ss"
         "../text-area.ss"
         "../text-field.ss"
         "../tiny-mce.ss"
         "check-label.ss"
         "util.ss")

; Interfaces -------------------------------------

(define snooze-form-element<%>
  (interface (form-element<%> check-label<%>)
    get-predicate
    set-predicate!
    get-wrapper-id))

; Mixins -----------------------------------------

(define snooze-form-element-mixin
  (mixin/cells (form-element<%> check-label<%>) (snooze-form-element<%>)
    
    (inherit get-id
             render-check-label)
    
    ; Fields -------------------------------------
    
    ; (cell (check-result -> boolean))
    (init-cell [predicate (lambda (result) #t)]
               #:accessor #:mutator)
    
    ; Methods ------------------------------------
    
    ; check-result -> boolean
    (define/override (report-result? result)
      (or (memq this (check-result-annotation result ann:form-elements))
          ((get-predicate) result)))
    
    ; -> symbol
    (define/public (get-wrapper-id)
      (symbol-append (get-id) '-wrapper))
    
    ; seed -> xml
    (define/override (render seed)
      (define id (get-wrapper-id))
      (xml (span (@ [id ,id])
                 ,(super render seed) " "
                 ,(render-check-label seed))))
    
    ; seed -> js
    (define/override (get-on-render seed)
      (js (!dot Smoke (insertHTML (!dot Smoke (findById ,(get-wrapper-id)))
                                  "replace"
                                  ,(xml->string (render seed))))))))

; Classes ----------------------------------------

(define snooze-autocomplete-field%        (snooze-form-element-mixin (check-label-mixin autocomplete-field%)))
(define snooze-check-box%                 (snooze-form-element-mixin (check-label-mixin check-box%)))
(define snooze-combo-box%                 (snooze-form-element-mixin (check-label-mixin combo-box%)))
(define snooze-vanilla-combo-box%         (snooze-form-element-mixin (check-label-mixin vanilla-combo-box%)))
(define snooze-date-field%                (snooze-form-element-mixin (check-label-mixin date-field%)))
(define snooze-file-field%                (snooze-form-element-mixin (check-label-mixin file-field%)))
(define snooze-integer-field%             (snooze-form-element-mixin (check-label-mixin integer-field%)))
(define snooze-password-field%            (snooze-form-element-mixin (check-label-mixin password-field%)))
(define snooze-regexp-field%              (snooze-form-element-mixin (check-label-mixin regexp-field%)))
(define snooze-set-selector%              (snooze-form-element-mixin (check-label-mixin set-selector%)))
(define snooze-set-selector-autocomplete% (snooze-form-element-mixin (check-label-mixin set-selector-autocomplete%)))
(define snooze-text-area%                 (snooze-form-element-mixin (check-label-mixin text-area%)))
(define snooze-text-field%                (snooze-form-element-mixin (check-label-mixin text-field%)))
(define snooze-tiny-mce%                  (snooze-form-element-mixin (check-label-mixin tiny-mce%)))

; Provide statements -----------------------------

(provide snooze-form-element<%>
         snooze-form-element-mixin
         snooze-autocomplete-field%
         snooze-check-box%
         snooze-combo-box%
         snooze-vanilla-combo-box%
         snooze-date-field%
         snooze-file-field%
         snooze-integer-field%
         snooze-password-field%
         snooze-regexp-field%
         snooze-set-selector%
         snooze-set-selector-autocomplete%
         snooze-text-area%
         snooze-text-field%
         snooze-tiny-mce%)
