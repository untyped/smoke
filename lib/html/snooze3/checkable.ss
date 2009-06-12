#lang scheme/base

(require "../../../lib-base.ss")

; Interfaces -------------------------------------

(define checkable<%>
  (interface ()
    set-check-results!)) ; (listof check-result) -> void

; Provide statements -----------------------------

(provide checkable<%>)
