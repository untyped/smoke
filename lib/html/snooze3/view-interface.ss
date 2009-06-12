#lang scheme/base

(require "../../../lib-base.ss"
         "../html-component.ss")

; Interfaces -------------------------------------

(define view<%>
  (interface (html-component<%>)
    get-views      ; -> (listof view<%>)
    get-value      ; -> any
    set-value!     ; any -> void
    render-value)) ; seed -> xml

; Provide statements -----------------------------

(provide view<%>)
