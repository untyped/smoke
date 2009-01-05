#lang scheme/base

(require (for-syntax scheme/base)
         scheme/contract
         scheme/match
         scheme/pretty
         scheme/runtime-path
         srfi/26
         (planet untyped/dispatch:1/dispatch)
         (planet untyped/mirrors:1/mirrors)
         "../dispatch.ss"
         "../smoke.ss"
         "../lib/html/snooze/snooze.ss")

; path
(define-runtime-path testapp-htdocs-path "htdocs")

; Provide statements -----------------------------

(provide (all-from-out scheme/contract
                       scheme/match
                       scheme/pretty
                       srfi/26
                       (planet untyped/dispatch:1/dispatch)
                       (planet untyped/mirrors:1/mirrors)
                       "../dispatch.ss"
                       "../smoke.ss"
                       "../lib/html/snooze/snooze.ss"))

(provide/contract
 [testapp-htdocs-path path?])
