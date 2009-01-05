#lang scheme/base

(require (planet untyped/snooze:2/check/check)
         "../../../lib-base.ss"
         "../browser-util.ss"
         "../form-element.ss"
         "../html-element.ss"
         "../notification.ss"
         "check-label.ss"
         "util.ss")

; Interfaces -------------------------------------

; interface
(define snooze-editor<%>
  (interface ()
    parse            ; -> (listof check-result)
    validate         ; -> (listof check-result)
    on-update        ; -> any
    commit-changes   ; -> any
    value-changed?)) ; -> boolean

; Mixins -----------------------------------------

(define snooze-editor-mixin
  (mixin/cells (notification-element<%>) (snooze-editor<%>)
    
    (inherit get-child-components
             add-notification!)
    
    ; Methods ------------------------------------
    
    ; -> (listof (U xml (seed -> xml)))
    (define/augment (get-html-requirements)
      (list* snooze-styles
             (inner null get-html-requirements)))
    
    ; -> (listof check-result)
    (define/public (parse)
      (apply check-problems
             (for/list ([child (get-child-components)])
               (define message (and (is-a? child form-element<%>) (send child get-value-error)))
               (check/annotate ([ann:form-elements (list child)])
                 (if message
                     (check-fail message)
                     (check-pass))))))
    
    ; -> (listof check-result)
    (define/public (validate)
      (error "validate must be overridden."))
    
    ; -> any
    (define/public (commit-changes)
      (error "commit-changes must be overridden."))
    
    ; -> any
    (define/public #:callback/return (on-update)
      (process-parse-results (parse)))    
    
    ; -> xml
    (define/public (get-warning-notification)
      (xml "The data you submitted has raised warnings. "
           "Hover over the " ,warning-icon " icons below to see the warning messages. "
           "Click \"OK\" again if you are satisfied that the data is correct."))
    
    ; -> xml
    (define/public (get-failure-notification)
      (xml "The data you submitted contains mistakes. "
           "Hover over the " ,failure-icon " icons below to see the error messages. "
           "Correct the mistakes and click \"OK\" again to submit your changes."))
    
    (define/public (get-commit-notification)
      (xml "Your changes have been saved successfully."))
    
    ; (listof check-result) -> any
    (define/private (process-parse-results results)
      (update-check-labels! results)
      (cond [(ormap check-warning? results) (print-check-fatals results #:id (debug-id this))
                                            (error (format "parse! method produced check-warnings:~n~s"
                                                           (with-pretty-indent "  "
                                                                               (pretty-format results))))
                                            (send (current-page) respond)]
            [(ormap check-fatal? results)   (print-check-fatals results #:id (debug-id this))
                                            (send (current-page) respond)]
            [(ormap check-failure? results) (add-notification! (get-failure-notification))
                                            (send (current-page) respond)]
            [else                           (process-validate-results (validate))]))
    
    ; (listof check-result) -> any
    (define/private (process-validate-results results)
      (update-check-labels! results)
      (cond [(ormap check-fatal? results)   (print-check-fatals results #:id (debug-id this))
                                            (send (current-page) respond)]
            [(ormap check-failure? results) (add-notification! (get-failure-notification))
                                            (send (current-page) respond)]
            [(ormap check-warning? results) (if (value-changed?)
                                                (begin
                                                  (add-notification! (get-warning-notification))
                                                  (send (current-page) respond))
                                                (begin0
                                                  (commit-changes)
                                                  (add-notification! (get-commit-notification))))]
            [else                           (begin0
                                              (commit-changes)
                                              (add-notification! (get-commit-notification)))]))
    
    ; (listof check-result) -> void
    (define/public (update-check-labels! results)
      (map (cut send <> set-results! results)
           (filter (cut is-a? <> check-label<%>)
                   (get-child-components))))
    
    ; -> boolean
    (define/public (value-changed?)
      (ormap (lambda (component)
               (or (and (is-a? component form-element<%>)
                        (send component value-changed?))
                   (and (is-a? component snooze-editor<%>)
                        (send component value-changed?))))
             (get-child-components)))))

; Classes ----------------------------------------

(define snooze-editor%
  (class/cells (snooze-editor-mixin (notification-mixin html-element%)) ()))

; Helpers ----------------------------------------

; component<%> -> symbol
(define (debug-id obj)
  (with-handlers ([exn? (lambda _ (send obj get-component-id))])
    (send obj get-id)))

; Provide statements -----------------------------

(provide (all-from-out "util.ss")
         snooze-editor<%>
         snooze-editor-mixin
         snooze-editor%)
