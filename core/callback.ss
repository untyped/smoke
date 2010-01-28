#lang scheme

(require "json.ss")

; (struct)
(define-struct seed () #:transparent)

; (struct html-component<%> symbol (listof json-serializable))
(define-struct callback (component callback-id args) #:transparent)

; Provides ---------------------------------------

(provide/contract
 [struct seed     ()]
 [struct callback ([component any/c] [callback-id symbol?] [args (listof (or/c symbol? json-serializable?))])])