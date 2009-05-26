#lang scheme/base

(require (planet untyped/snooze:3)
         "../../../../lib-base.ss"
         "../../html-element.ss"
         "default-abstract.ss"
         "scaffold-internal.ss")

; Mixins -----------------------------------------

(define (default-list-mixin)
  (lambda (n) n))  

; Provides ---------------------------------------
(provide (all-defined-out))