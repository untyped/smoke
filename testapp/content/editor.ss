#lang scheme/base

(require "../content-base.ss")

(require (planet untyped/snooze:2)
         (planet untyped/snooze:2/check/check)
         (planet untyped/unlib:3/list)
         (planet untyped/unlib:3/symbol))

; Controllers ------------------------------------

; request -> response
(define-controller (editor request)
  (send editor-page respond))

; Helpers ----------------------------------------

(define (field-html field seed)
  (xml (tr (td ,(send field get-id))
           (td ,(send field render seed))
           (td ,(with-handlers ([exn? (lambda _ "[parse error]")])
                  (send field get-value))))))

; annotation
(define-annotation ann:key
  (lambda (result) #f)
  (lambda (result old new) new))

; -> (any -> boolean)
(define (by-key key)
  (lambda (result)
    (eq? (check-result-annotation result ann:key) key)))

; natural
(define total-commits
  0)

; Components -------------------------------------

(define editor-page
  (singleton/cells (snooze-editor-mixin html-page%) ()
    
    ; Fields -------------------------------------
    
    (field notification-pane
      (new notification-pane% [id 'notifications])
      #:child #:accessor #:mutator)
    
    (field larger-field
      (new snooze-integer-field% [id 'larger-field] [value 1] [predicate (by-key 'integer-fields)])
      #:child #:accessor #:mutator)
    
    (field smaller-field
      (new snooze-integer-field% [id 'smaller-field] [value 2] [predicate (by-key 'integer-fields)])
      #:child #:accessor #:mutator)
    
    (field submit-button
      (new submit-button% [id 'submit-button] [action (callback on-update)])
      #:child #:accessor #:mutator)
    
    ; Constructor --------------------------------
    
    (super-new [title "Editor"])
    
    ; Methods ------------------------------------
    
    ; seed -> xml
    (define/augment (render seed)
      (xml ,(send notification-pane render seed)
           (table (tr (th "ID")
                      (th "Widget")
                      (th "Value"))
                  ,(field-html larger-field seed)
                  ,(field-html smaller-field seed)
                  (tr (td (@ [colspan 3])
                          ,(send submit-button render seed))))
           (p (span (@ [id "total-commits"]) ,total-commits) " successful commits so far")))
    
    ; -> (listof check-result)
    (define/override (validate)
      (check-all (check/annotate ([ann:key 'integer-fields])
                   (let ([larger  (send larger-field get-value)]
                         [smaller (send smaller-field get-value)])
                     (cond [(> larger smaller) (check-pass)]
                           [(= larger smaller) (check-warn "larger-field is the same as smaller-field")]
                           [else               (check-fail "larger-field is smaller than smaller-field")])))))
    
    ; -> any
    (define/override (commit-changes)
      (set! total-commits (add1 total-commits))
      (send (current-page) respond))))
