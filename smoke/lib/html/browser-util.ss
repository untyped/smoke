#lang scheme/base

(require "../../lib-base.ss")

; XML requirements -------------------------------

(define blueprint-styles
  (xml (link (@ [rel   "stylesheet"] 
                [href  "/style/blueprint/screen.css"]
                [type  "text/css"]
                [media "screen, projection"]))
       (link (@ [rel   "stylesheet"]
                [href  "/style/blueprint/print.css"]
                [type  "text/css"]
                [media "print"]))
       (!raw "<!--[if IE]>")
       (link (@ [rel   "stylesheet"]
                [href  "/style/blueprint/ie.css"]
                [type  "text/css"]
                [media "screen, projection"]))
       (!raw "<![endif]-->")))

(define smoke-styles
  (xml (link (@ [rel   "stylesheet"] 
                [href  "/style/smoke/screen.css"]
                [type  "text/css"]
                [media "screen, projection"]))
       (link (@ [rel   "stylesheet"]
                [href  "/style/smoke/print.css"]
                [type  "text/css"]
                [media "print"]))
       (!raw "<!--[if IE]>")
       (link (@ [rel   "stylesheet"]
                [href  "/style/smoke/ie.css"]
                [type  "text/css"]
                [media "screen, projection"]))
       (!raw "<![endif]-->")))

(define tooltip-script
  (xml (script (@ [type "text/javascript"]
                  [src  "/scripts/smoke/tooltip.js"]))
       (link (@ [rel   "stylesheet"]
                [href  "/style/smoke/tooltip.css"]
                [type  "text/css"]
                [media "screen, projection"]))))

(define rollover-script
  (xml (script (@ [type "text/javascript"]
                  [src  "/scripts/smoke/rollover.js"]))))

(define show-hide-script
  (xml (script (@ [type "text/javascript"]
                  [src  "/scripts/smoke/show-hide.js"]))
       (link (@ [rel   "stylesheet"]
                [href  "/style/smoke/show-hide.css"]
                [type  "text/css"]
                [media "screen, projection"]))))

(define excanvas-script
  (xml (!raw "<!--[if IE]>")
       (script (@ [type "text/javascript"]
                  [src  "/scripts/excanvas/excanvas.js"]))
       (!raw "<![endif]-->")))

(define firebug-script
  (xml (script (@ [type "text/javascript"]
                  [src  "/scripts/firebug/firebug-lite.js"]))))

; Javascript requirements ------------------------

; Provide statements -----------------------------

(provide/contract
 [blueprint-styles xml?]
 [smoke-styles     xml?]
 [tooltip-script   xml?]
 [rollover-script  xml?]
 [show-hide-script xml?]
 [excanvas-script  xml?]
 [firebug-script   xml?])
