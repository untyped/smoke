#lang scheme

; This value and predicate are used in init-cell statements,
; to help save memory in web frames.
;
; A natural way to implement init-cell might be:
;
;   (field [my-field-cell (make-web-cell #f)])
;   (init  [my-field expression])
;   (web-cell-set! my-field-cell expression))
;
; However, the call to web-cell-set! stores a value in the web frame
; that is costly to store. Instead we do the following:
;
;   (field [my-field-cell (make-web-cell expression)])
;   (init  [my-field undefined])
;   (unless (undefined? my-field)
;     (web-cell-set! my-field-cell my-field))
;
; This stores the default expresion value in the web-cell itself,
; saving space in the web-frame unless the init argument is explicitly used.

; undefined
(define undefined
  (letrec ([x x]) x))

; any -> boolean
(define (undefined? x)
  (eq? x undefined))

; Provides ---------------------------------------

(provide undefined undefined?)