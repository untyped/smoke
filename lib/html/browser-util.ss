#lang scheme/base

(require "../../lib-base.ss")

; XML requirements -------------------------------

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

(define tiny-mce-script
  (xml (script (@ [type "text/javascript"]
                  [src  "/scripts/tiny_mce/tiny_mce.js"]))))

; Javascript requirements ------------------------

(define default-tiny-mce-options-script
  (xml (script (@ [type "text/javascript"])
               (!raw "\n// <![CDATA[\n")
               (!raw ,(js (!dot tinyMCE (init (!object [mode  "none"]
                                                       [style "simple"])))))
               (!raw "\n// ]]>\n"))))

; Provide statements -----------------------------

(provide/contract
 [smoke-styles                    xml?]
 [tooltip-script                  xml?]
 [rollover-script                 xml?]
 [show-hide-script                xml?]
 [excanvas-script                 xml?]
 [firebug-script                  xml?]
 [tiny-mce-script                 xml?]
 [default-tiny-mce-options-script xml?])
