#lang scheme/base

(require "../../../lib-base.ss"
         "../html-page.ss")

; Variables --------------------------------------

; (parameter (subclassof html-page%))

(define default-scaffolded-page-superclass
  (make-parameter html-page%))

; Provide statements -----------------------------

(provide/contract
 [default-scaffolded-page-superclass (parameter/c (subclass?/c html-page%))])