#lang scheme

(require web-server/http
         web-server/private/connection-manager
         "interfaces.ss")

; Current connection -----------------------------

; (thread-cell (U connection #f))
(define current-connection-cell (make-thread-cell #f))

; request -> void
(define (current-connection-set! conn)
  (thread-cell-set! current-connection-cell conn))

; natural -> void
(define (adjust-http-timeout! timeout)
  (adjust-connection-timeout! (thread-cell-ref current-connection-cell) timeout))

; Current request --------------------------------

; (thread-cell (U request #f))
(define current-request-cell (make-thread-cell #f))

; request -> void
(define (current-request-set! request)
  (thread-cell-set! current-request-cell request))

; -> (U request #f)
(define (current-request)
  (thread-cell-ref current-request-cell))

; Current request --------------------------------

; (thread-cell (U request #f))
(define current-site-cell (make-thread-cell #f))

; request -> void
(define (current-site-set! app)
  (thread-cell-set! current-site-cell app))

; -> (U request #f)
(define (current-site)
  (thread-cell-ref current-site-cell))

; Current page -----------------------------------

; (thread-cell (U request #f))
(define current-page-cell (make-thread-cell #f))

; request -> void
(define (current-page-set! app)
  (thread-cell-set! current-page-cell app))

; -> (U request #f)
(define (current-page)
  (thread-cell-ref current-page-cell))

; History ----------------------------------------

; This is here because I haven't thought of a place to put it yet:

; -> void
(define (clear-history!)
  (printf "clear-history! not yet implemented~n"))

; Provide statements -----------------------------

(provide/contract
 [adjust-http-timeout!     (-> natural-number/c void?)]
 [current-connection-set!  (-> connection? void?)]
 [current-request          (-> (or/c request? #f))]
 [current-request-set!     (-> request? void?)]
 [current-site             (-> (or/c (is-a?/c site<%>) #f))]
 [current-site-set!        (-> (is-a?/c site<%>) void?)]
 [current-page             (-> (or/c (is-a?/c page<%>) #f))]
 [current-page-set!        (-> (is-a?/c page<%>) void?)]
 [clear-history!           (-> void?)])
