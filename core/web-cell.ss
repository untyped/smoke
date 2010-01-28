#lang scheme

(require web-server/servlet/web-cells
         (planet untyped/unlib:3/debug)
         (planet untyped/unlib:3/exn))

; Struct types -----------------------------------

; (parameter boolean)
(define use-old-web-frame?
  (make-parameter #f))

; (_ expr ...)
(define-syntax-rule (with-old-web-frame expr ...)
  (parameterize ([use-old-web-frame? #t])
    expr ...))

; (struct web-cell (thread-cell (cons boolean any)))
(define-struct wrapper (data changed) #:transparent)

; Procedures -------------------------------------

; any -> wrapper
(define (create-wrapper default)
  (make-wrapper (make-web-cell default)
                (make-thread-cell (cons #f #f))))

; wrapper -> any
(define (wrapper-ref cell)
  (if (use-old-web-frame?)
      (wrapper-old cell)
      (wrapper-new cell)))

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
(define (wrapper-new cell)
  (web-cell-ref (wrapper-data cell)))

; wrapper -> any
(define (wrapper-old cell)
  (let ([pair (thread-cell-ref (wrapper-changed cell))])
    (if (car pair)
        (cdr pair)
        (wrapper-new cell))))

; wrapper -> any
(define (wrapper-changed? cell)
  (let ([pair (thread-cell-ref (wrapper-changed cell))])
    (car pair)))

; Provide statements ---------------------------

(provide (rename-out [web-cell-set?         web-frame?]
                     [capture-web-cell-set  capture-web-frame]
                     [restore-web-cell-set! restore-web-frame!])
         with-old-web-frame)

(provide/contract
 [rename create-wrapper   make-web-cell     (-> any/c wrapper?)]
 [rename wrapper?         web-cell?         (-> any/c boolean?)]
 [rename wrapper-ref      web-cell-ref      (-> wrapper? any)]
 [rename wrapper-set!     web-cell-set!     (-> wrapper? any/c void?)]
 [rename wrapper-set?     web-cell-set?     (-> wrapper? boolean?)]
 [rename wrapper-changed? web-cell-changed? (-> wrapper? boolean?)])
