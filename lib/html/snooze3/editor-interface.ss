#lang scheme/base

(require "../../../lib-base.ss"
         "../form-element.ss")

; Interfaces -------------------------------------

(define editor<%>
  (interface (form-element<%>)
    get-editors ; -> (listof editor<%>)
    parse       ; -> (listof check-result)
    validate))  ; -> (listof check-result)

; Provide statements -----------------------------

(provide editor<%>)
