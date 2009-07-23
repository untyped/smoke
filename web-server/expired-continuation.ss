#lang scheme/base

(require scheme/contract
         srfi/26
         (planet untyped/unlib:3/enumeration)
         "session-cell.ss")

; enum
(define-enum expired-continuation-types
  (get post ajax))

; (cell expired-continuation-type)
(define expired-continuation-type-cell
  (make-session-cell #f))

; -> (U expired-continuation-type #f)
(define (expired-continuation-type)
  (session-cell-ref expired-continuation-type-cell))

; -> void
(define (expired-continuation-type-set! type)
  (session-cell-set! expired-continuation-type-cell type))
; -> void
(define (expired-continuation-type-reset!)
  (session-cell-set! expired-continuation-type-cell #f))

; Provide statements -----------------------------

(provide expired-continuation-types)

(provide/contract
 [expired-continuation-type        (-> (or/c (cut enum-value? expired-continuation-types <>) #f))]
 [expired-continuation-type-set!   (-> (or/c (cut enum-value? expired-continuation-types <>) #f) void?)]
 [expired-continuation-type-reset! (-> void?)])
