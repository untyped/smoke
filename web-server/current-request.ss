#lang scheme/base

(require net/url
         scheme/contract
         web-server/http
         web-server/private/connection-manager)

; Current connection -----------------------------

; (thread-cell (U connection #f))
(define current-connection-cell
  (make-thread-cell #f))

; request -> void
(define (current-connection-set! conn)
  (thread-cell-set! current-connection-cell conn))

; natural -> void
(define (adjust-http-timeout! timeout)
  (adjust-connection-timeout! (thread-cell-ref current-connection-cell) timeout))

; Current request --------------------------------

; (thread-cell (U request #f))
(define current-request-cell
  (make-thread-cell #f))

; request -> void
(define (current-request-set! request)
  (thread-cell-set! current-request-cell request))

; -> (U request #f)
(define (current-request)
  (thread-cell-ref current-request-cell))

; Provide statements -----------------------------

(provide/contract
 [adjust-http-timeout!    (-> natural-number/c void?)]
 [current-connection-set! (-> connection? void?)]
 [current-request         (-> (or/c request? #f))]
 [current-request-set!    (-> request? void?)])
