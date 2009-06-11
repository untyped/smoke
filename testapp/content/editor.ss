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
  (singleton/cells (editor-page-mixin html-page%) ()
    
    ; Fields -------------------------------------
    
    (field notification-pane (new notification-pane%) #:child)
    
    ; Constructor --------------------------------
    
    (super-new [entity kitchen-sink])
    
    ; Methods ------------------------------------
    
    ; seed -> xml
    (define/override (render seed)
      (xml ,(send notification-pane render seed)
           ,(super render seed)))))
