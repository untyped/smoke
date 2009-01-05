#lang scheme/base

(require "plan.ss"
         "struct.ss")

; web-resource
(define-js-resource jquery-script
  "/scripts/jquery/jquery.js")

; web-resource
(define-js-resource smoke-script
  "/scripts/smoke/smoke.js"
  (jquery-script))

; web-resource
(define-js-resource tooltip-script
  "/scripts/smoke/tooltip.js"
  (smoke-script jquery-script))

; web-resource
(define-js-resource rollover-script
  "/scripts/smoke/rollover.js"
  (smoke-script jquery-script))

(define-compound-resource all-scripts
  (jquery-script
   smoke-script
   tooltip-script
   rollover-script))
