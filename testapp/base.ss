#lang scheme/base

(require (for-syntax scheme/base)
         scheme/contract
         scheme/match
         scheme/pretty
         scheme/runtime-path
         srfi/26
         (planet untyped/mirrors:2)
         (planet untyped/unlib:3/debug)
         "../main.ss")

; path
(define-runtime-path testapp-htdocs-path "htdocs")

; Provide statements -----------------------------

(provide (all-from-out scheme/contract
                       scheme/match
                       scheme/pretty
                       srfi/26
                       (planet untyped/mirrors:2)
                       (planet untyped/unlib:3/debug)
                       "../main.ss"))

(provide/contract
 [testapp-htdocs-path path?])
