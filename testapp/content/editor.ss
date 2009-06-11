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
     (let ([entity (send (send editor-page get-editor) get-entity)])
       (let loop ([struct ((entity-defaults-constructor entity))])
         (send (send editor-page get-editor) set-value! struct)
         (loop (send editor-page respond)))))))

; Helpers ----------------------------------------

; natural
(define total-commits 0)

; Components -------------------------------------

(define editor-page
  (singleton/cells html-page% ()
    
    ; Fields -------------------------------------
    
    (field notification-pane
      (new notification-pane%)
      #:child)
    
    (field editor
      (new entity-editor% [entity kitchen-sink])
      #:child #:accessor)
    
    (field submit-button
      (new submit-button% [action (callback [editor on-update])])
      #:child)
    
    ; Constructor --------------------------------
    
    (super-new [title (format "~a Editor" (string-titlecase (entity-pretty-name (send editor get-entity))))])
    
    ; Methods ------------------------------------
    
    ; seed -> xml
    (define/augment (render seed)
      (xml ,(send notification-pane render seed)
           ,(send editor render seed)
           ,(send submit-button render seed)))))
