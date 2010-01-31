#lang scheme

(require "interfaces.ss"
         "json.ss"
         "../class/class.ss")

; (struct)
(define-struct seed () #:transparent)

; We can create a callback struct containing a component or a component ID.
; We really only need the ID, but storing a compoment is useful for delaying
; the evaluation of its ID so we don't need to worry so hard about the order
; of (field ...) and (super-new ...) statements in components.
;
; (struct (U component<%> symbol) symbol (listof json-serializable))
(define-struct callback (component+id method-id args) #:transparent)

; callback -> symbol
(define (callback-component-id callback)
  (let ([ans (callback-component+id callback)])
    (if (is-a? ans component<%>)
        (send ans get-component-id)
        ans)))

; Provides ---------------------------------------

(provide/contract
 [struct seed           ()]
 [struct callback       ([component+id (or/c (is-a?/c component<%>) symbol?)]
                         [method-id    symbol?]
                         [args         (listof (or/c symbol? json-serializable?))])]
 [callback-component-id (-> callback? symbol?)])