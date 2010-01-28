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
(define current-application-cell (make-thread-cell #f))

; request -> void
(define (current-application-set! app)
  (thread-cell-set! current-application-cell app))

; -> (U request #f)
(define (current-application)
  (thread-cell-ref current-application-cell))

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
 [current-application      (-> (or/c (is-a?/c application<%>) #f))]
 [current-application-set! (-> (is-a?/c application<%>) void?)]
 [current-page             (-> (or/c (is-a?/c page<%>) #f))]
 [current-page-set!        (-> (is-a?/c page<%>) void?)]
 [clear-history!           (-> void?)])
