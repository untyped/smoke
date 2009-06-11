#lang scheme/base

(require (planet untyped/snooze:3)
         "../../../lib-base.ss"
         "../disableable-element.ss"
         "../html-element.ss"
         "../notification.ss"
         "check-label.ss"
         "editor-interface.ss"
         "entity-editor.ss"
         "util.ss")

; Interfaces -------------------------------------

; interface
(define editor-controller<%>
  (interface ()
    on-update        ; -> any
    commit-changes   ; -> any
    value-changed?)) ; -> boolean

; Mixins -----------------------------------------

(define editor-controller-mixin
  (mixin/cells (editor<%>) (editor-controller<%>)
    
    (inherit get-child-components
             get-value
             parse
             validate
             value-changed?)
    
    ; Methods ------------------------------------
    
    ; -> (listof (U xml (seed -> xml)))
    (define/augment (get-html-requirements)
      (list* snooze-styles
             (inner null get-html-requirements)))
    
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
    
    ; (listof check-result) -> void
    (define/public (update-check-labels! results)
      (map (cut send <> set-results! results)
           (filter (cut is-a? <> check-label<%>)
                   (get-child-components))))
    
    ; -> any
    (define/public #:callback/return (on-update)
      (process-parse-results (parse)))
    
    ; (listof check-result) -> any
    (define/private (process-parse-results results)
      (update-check-labels! results)
      (cond [(ormap check-warning? results) (print-check-fatals results #:id (debug-id this))
                                            (error (format "parse! method produced check-warnings:~n~s"
                                                           (with-pretty-indent "  " (pretty-format results))))
                                            (send (current-page) respond)]
            [(ormap check-fatal? results)   (print-check-fatals results #:id (debug-id this))
                                            (send (current-page) respond)]
            [(ormap check-failure? results) (notifications-add! (get-failure-notification))
                                            (send (current-page) respond)]
            [else                           (process-validate-results (validate))]))
    
    ; (listof check-result) -> any
    (define/private (process-validate-results results)
      (update-check-labels! results)
      (cond [(ormap check-fatal? results)   (print-check-fatals results #:id (debug-id this))
                                            (send (current-page) respond)]
            [(ormap check-failure? results) (notifications-add! (get-failure-notification))
                                            (send (current-page) respond)]
            [(ormap check-warning? results) (if (value-changed?)
                                                (begin  (notifications-add! (get-warning-notification))
                                                        (send (current-page) respond))
                                                (begin0 (commit-changes)
                                                        (notifications-add! (get-commit-notification))))]
            [else                           (begin0 (commit-changes)
                                                    (notifications-add! (get-commit-notification)))]))
    
    ; -> any
    (define/public (commit-changes)
      (printf "committing changes~n")
      (if (is-a? this entity-editor<%>)
          (save! (get-value))
          (error "editor is not an entity-editor: commit-changes must be overridden.")))))

; Classes ----------------------------------------

(define entity-editor%
  (editor-controller-mixin (entity-editor-mixin (disableable-element-mixin html-element%))))

; Helpers ----------------------------------------

; component<%> -> symbol
(define (debug-id obj)
  (with-handlers ([exn? (lambda _ (send obj get-component-id))])
    (send obj get-id)))

; Provide statements -----------------------------

(provide (all-from-out "util.ss")
         editor-controller<%>
         editor-controller-mixin
         entity-editor%)
