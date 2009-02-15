#lang scheme/base

(require net/url
         scheme/contract
         web-server/http)

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
 [current-request-cell thread-cell?]
 [current-request      (-> (or/c request? false/c))]
 [current-request-set! (-> request? void?)])
