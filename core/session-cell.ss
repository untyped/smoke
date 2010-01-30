#lang scheme

(require "env.ss"
         "session.ss")

; Structure types --------------------------------

; (struct (hashof string any))
(define-struct session-cell (hash default) #:transparent)

; Procedures -------------------------------------

; any -> session-cell
(define (create-session-cell default)
  (make-session-cell (make-hash) default))

; sesion-cell -> any
(define (session-cell-ref cell)
  (hash-ref (session-cell-hash cell)
            (request-session-id (current-request))
            (session-cell-default cell)))

; sesion-cell any -> void
(define (session-cell-set! cell val)
  (hash-set! (session-cell-hash cell)
             (request-session-id (current-request))
             val))

; sesion-cell -> void
(define (session-cell-unset! cell)
  (hash-remove! (session-cell-hash cell)
                (request-session-id (current-request))))

; Provide statements -----------------------------

(provide/contract
 [rename create-session-cell make-session-cell (-> any/c session-cell?)]
 [session-cell?                                (-> any/c boolean?)]
 [session-cell-ref                             (-> session-cell? any)]
 [session-cell-set!                            (-> session-cell? any/c void?)]
 [session-cell-unset!                          (-> session-cell? void?)])
