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
  (printf "SET current-request ~a~n" (and request (url->string (request-uri request))))
  (thread-cell-set! current-request-cell request))

; -> (U request #f)
(define (current-request)
  ;(printf "REF current-request ~a~n" (thread-cell-ref current-request-cell))
  (thread-cell-ref current-request-cell))

; Provide statements -----------------------------

(provide/contract
 [current-request-cell thread-cell?]
 [current-request      (-> (or/c request? false/c))]
 [current-request-set! (-> request? void?)])
