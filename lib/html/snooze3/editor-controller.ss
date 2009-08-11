#lang scheme/base

(require (planet untyped/snooze:3)
         "../../../lib-base.ss"
         "../notification.ss"
         "check-label.ss"
         "editor-internal.ss"
         "entity-editor.ss")

; Interfaces -------------------------------------

; interface
(define editor-controller<%>
  (interface ()
    get-editor       ; -> editor<%>
    set-editor!      ; editor<%> -> void
    on-update        ; -> any
    commit-changes)) ; -> any

; Mixins -----------------------------------------

(define editor-controller-mixin
  (mixin/cells () (editor-controller<%>)
    
    ; Fields -------------------------------------
    
    ; (cell editor<%>)
    (init-cell editor #:accessor #:mutator)
    
    ; Methods ------------------------------------
    
    ; (listof check-result) -> xml
    (define/public (get-warning-notification results)
      (xml "The data you submitted has raised warnings. Hover over the "
           (span (@ [class "tooltip-anchor"])
                 ,warning-icon
                 (div (@ [class "tooltip"])
                      (ul ,@(for/list ([result (in-list results)])
                              (xml (li ,(check-result-message result)))))))
           " icons below to see the warning messages. Submit your changes again if you are satisfied that the data is correct."))
    
    ; (listof check-result) -> xml
    (define/public (get-failure-notification results)
      (xml "The data you submitted contains mistakes. Hover over the "
           (span (@ [class "tooltip-anchor"])
                 ,failure-icon
                 (div (@ [class "tooltip"])
                      (ul ,@(for/list ([result (in-list results)])
                              (xml (li ,(check-result-message result)))))))
           " icons below to see the error messages. Correct the mistakes and submit your changes again."))
    
    (define/public (get-commit-notification)
      (xml "Your changes have been saved successfully."))
    
    ; -> any
    (define/public #:callback/return (on-update)
      (process-parse-results (send (get-editor) parse)))
    
    ; (listof check-result) -> any
    (define/private (process-parse-results results)
      (send (get-editor) set-check-results! results)
      (cond [(ormap check-warning? results) (print-check-fatals results #:id (debug-id this))
                                            (error (format "parse! method produced check-warnings:~n~s"
                                                           (with-pretty-indent "  " (pretty-format results))))
                                            (send (current-page) respond)]
            [(ormap check-fatal? results)   (print-check-fatals results #:id (debug-id this))
                                            (send (current-page) respond)]
            [(ormap check-failure? results) (notifications-add! (get-failure-notification results))
                                            (send (current-page) respond)]
            [else                           (process-validate-results (send (get-editor) validate))]))
    
    ; (listof check-result) -> any
    (define/private (process-validate-results results)
      (send (get-editor) set-check-results! results)
      (cond [(ormap check-fatal? results)   (print-check-fatals results #:id (debug-id this))
                                            (send (current-page) respond)]
            [(ormap check-failure? results) (notifications-add! (get-failure-notification results))
                                            (send (current-page) respond)]
            [(ormap check-warning? results) (if (send (get-editor) value-changed?)
                                                (begin  (notifications-add! (get-warning-notification results))
                                                        (send (current-page) respond))
                                                (begin0 (send (get-editor) commit-changes)
                                                        (notifications-add! (get-commit-notification))))]
            [else                           (begin0 (send (get-editor) commit-changes)
                                                    (notifications-add! (get-commit-notification)))]))
    
    ; -> any
    (define/public (commit-changes)
      (error "commit-changes must be overridden"))))

; Helpers ----------------------------------------

; component<%> -> symbol
(define (debug-id obj)
  (with-handlers ([exn? (lambda _ (send obj get-component-id))])
    (send obj get-id)))

; Provide statements -----------------------------

(provide editor-controller<%>
         editor-controller-mixin)
