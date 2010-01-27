#lang web-server

(require web-server/lang/web-cells
         (planet untyped/unlib:3/debug)
         (planet untyped/unlib:3/exn))

; Struct types -----------------------------------

; (struct web-cell thread-cell)
(define-struct wrapper (data changed) #:transparent)

; Procedures -------------------------------------

; any -> wrapper
(define (create-wrapper default)
  (make-wrapper (make-web-cell default)
                (make-thread-cell #f)))

; wrapper -> any
(define (wrapper-ref cell)
  (web-cell-ref (wrapper-data cell)))

; wrapper any -> void
(define (wrapper-set! cell val)
  (thread-cell-set! (wrapper-changed cell) #t)
  (web-cell-shadow (wrapper-data cell) val))

; wrapper -> boolean
(define (wrapper-set? cell)
  (with-handlers ([exn:fail? (lambda _ #f)])
    (web-cell-ref (wrapper-data cell))
    #t))

; wrapper -> any
(define (wrapper-changed? cell)
  (thread-cell-ref (wrapper-changed cell)))

; Provide statements ---------------------------

(provide/contract
 [rename create-wrapper   make-web-cell     (-> any/c wrapper?)]
 [rename wrapper?         web-cell?         (-> any/c boolean?)]
 [rename wrapper-ref      web-cell-ref      (-> wrapper? any)]
 [rename wrapper-set!     web-cell-set!     (-> wrapper? any/c void?)]
 [rename wrapper-set?     web-cell-set?     (-> wrapper? boolean?)]
 [rename wrapper-changed? web-cell-changed? (-> wrapper? boolean?)])
