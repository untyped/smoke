#lang scheme

(require "env.ss"
         #;"session.ss")

; Structure types --------------------------------

; (struct (hashof string any))
(define-struct session-cell (hash default) #:transparent)

; Procedures -------------------------------------

; any -> session-cell
(define (create-session-cell default)
  (make-session-cell (make-hash) default))

; sesion-cell -> any
(define (session-cell-ref cell)
  (printf "session-cell-ref needs to be implemented properly~n")
  (hash-ref (session-cell-hash cell)
            'abc123 #;(request-session-id (current-request))
            (session-cell-default cell)))

; sesion-cell any -> void
(define (session-cell-set! cell val)
  (printf "session-cell-set! needs to be implemented properly~n")
  (hash-set! (session-cell-hash cell)
             'abc123 #;(request-session-id (current-request))
             val))

; sesion-cell -> void
(define (session-cell-unset! cell)
  (printf "session-cell-unset! needs to be implemented properly~n")
  (hash-remove! (session-cell-hash cell)
                'abc123 #;(request-session-id (current-request))))

; Provide statements -----------------------------

(provide/contract
 [rename create-session-cell make-session-cell (-> any/c session-cell?)]
 [session-cell?                                (-> any/c boolean?)]
 [session-cell-ref                             (-> session-cell? any)]
 [session-cell-set!                            (-> session-cell? any/c void?)]
 [session-cell-unset!                          (-> session-cell? void?)])
