#lang scheme/base

(require "../../lib-base.ss")

; XML requirements -------------------------------

(define smoke-styles
  (xml (link (@ [rel   "stylesheet"] 
                [href  "/styles/smoke/screen.css"]
                [type  "text/css"]
                [media "screen, projection"]))
       (link (@ [rel   "stylesheet"]
                [href  "/styles/smoke/print.css"]
                [type  "text/css"]
                [media "print"]))
       (!raw "<!--[if IE]>")
       (link (@ [rel   "stylesheet"]
                [href  "/styles/smoke/ie.css"]
                [type  "text/css"]
                [media "screen, projection"]))
       (!raw "<![endif]-->")))

(define tooltip-script
  (xml (script (@ [type "text/javascript"]
                  [src  "/scripts/smoke/tooltip.js"]))
       (link (@ [rel   "stylesheet"]
                [href  "/styles/smoke/tooltip.css"]
                [type  "text/css"]
                [media "screen, projection"]))))

(define rollover-script
  (xml (script (@ [type "text/javascript"]
                  [src  "/scripts/smoke/rollover.js"]))))

(define show-hide-script
  (xml (script (@ [type "text/javascript"]
                  [src  "/scripts/smoke/show-hide.js"]))
       (link (@ [rel   "stylesheet"]
                [href  "/styles/smoke/show-hide.css"]
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

(define default-tiny-mce-options-script
  (xml (script (@ [type "text/javascript"])
               (!raw "\n// <![CDATA[\n")
               (!raw ,(js (!dot tinyMCE (init (!object [mode  "none"]
                                                       [style "simple"])))))
               (!raw "\n// ]]>\n"))))

; jQuery UI --------------------------------------

; (parameter string)
(define current-jquery-ui-stylesheet
  (make-parameter "/styles/jquery-ui/cupertino/jquery-ui-1.7.2.custom.css"))

(define jquery-ui-script
  (xml (script (@ [type "text/javascript"]
                  [src  "/scripts/jquery-ui/jquery-ui-1.7.1.custom.min.js"]))))

(define (jquery-ui-styles seed)
  (xml (link (@ [type "text/css"]
                [href ,(current-jquery-ui-stylesheet)]
                [rel  "stylesheet"]))))

; Provide statements -----------------------------

(provide/contract
 [smoke-styles                    xml?]
 [tooltip-script                  xml?]
 [rollover-script                 xml?]
 [show-hide-script                xml?]
 [excanvas-script                 xml?]
 [firebug-script                  xml?]
 [tiny-mce-script                 xml?]
 [current-jquery-ui-stylesheet    (parameter/c string?)]
 [jquery-ui-script                xml?]
 [jquery-ui-styles                (-> seed? xml?)]
 [default-tiny-mce-options-script xml?])
