#lang scheme/base

(require scheme/contract
         scheme/match
         (planet untyped/mirrors:2/xml/xml)
         (planet untyped/unlib:3/symbol))

; Notification structure -------------------------

; (struct symbol xml boolean)
(define-struct notification (id xml sticky?) #:transparent)

; xml boolean -> notification
(define (create-notification xml sticky?)
  (make-notification (gensym/interned 'n) xml sticky?))

; Provide statements -----------------------------

(provide/contract
 [struct notification ([id symbol?] [xml xml?] [sticky? boolean?])]
 [create-notification (-> xml? boolean? notification?)])
