#lang scheme/base

(require "../content-base.ss")

(require (planet untyped/snooze:3)
         (planet untyped/unlib:3/list)
         (planet untyped/unlib:3/symbol))

; Controllers ------------------------------------

; request -> response
(define-controller (editor request)
  (call-with-connection
   (lambda ()
     (send (send editor-page get-editor)
           set-value!
           (select-one #:from  post #:order ((asc post.guid))))
     (send editor-page respond)
     (editor (current-request)))))

; Helpers ----------------------------------------

; natural
(define total-commits 0)

; Components -------------------------------------

(define editor-page
  (singleton/cells html-page% ()
    
    ; Fields -------------------------------------
    
    (field subject-field
      (new integer-editor% [attributes (list (attr post subject))]))
    
    (field content-field
      (new integer-editor% [attributes (list (attr post content))]))

    (field notification-pane
      (new notification-pane%)
      #:child)
    
    (field editor (new entity-editor% [entity post])
      #:child #:accessor)
    
    (field submit-button
      (new submit-button% [action (callback [editor on-update])])
      #:child)
    
    ; Constructor --------------------------------
    
    (super-new [title "Editor"])
    
    ; Methods ------------------------------------
    
    ; seed -> xml
    (define/augment (render seed)
      (xml ,(send notification-pane render seed)
           ,(send editor render seed)
           ,(send submit-button render seed)))))
