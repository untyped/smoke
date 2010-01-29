#lang scheme

(require "json.ss"
         "../class/class.ss")

; (struct)
(define-struct seed () #:transparent)

; (struct symbol symbol (listof json-serializable))
(define-struct callback (component-id method-id args) #:transparent)

; Provides ---------------------------------------

(provide/contract
 [struct seed     ()]
 [struct callback ([component-id symbol?]
                   [method-id    symbol?]
                   [args         (listof (or/c symbol? json-serializable?))])])