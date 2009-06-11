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
     (let ([entity (send editor-page get-entity)])
       (let loop ([struct ((entity-defaults-constructor entity))])
         (send editor-page set-value! struct)
         (loop (send editor-page respond)))))))

; Components -------------------------------------

(define editor-page
  (new (entity-editor-page-mixin (render-augride-mixin html-page%))
       [entity kitchen-sink]))
