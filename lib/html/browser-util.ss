#lang scheme

(require "../../lib-base.ss")

; XML requirements -------------------------------

(define jquery-script/min
  (xml (script (@ [type "text/javascript"]
                  [src  "/scripts/jquery/jquery-1.3.2.min.js"]))))

(define jquery-script/dev
  (xml (script (@ [type "text/javascript"]
                  [src  "/scripts/jquery/jquery-1.3.2.js"]))))

(define smoke-script
  (xml (script (@ [type "text/javascript"]
                  [src  "/scripts/smoke/smoke.js"]))))

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
                  [src  "/scripts/smoke/ui.stackfix.js"]))
       (script (@ [type "text/javascript"]
                  [src  "/scripts/smoke/ui.position.js"]))
       (script (@ [type "text/javascript"]
                  [src  "/scripts/smoke/tooltip.js"]))))

(define picker-script
  (xml (script (@ [type "text/javascript"]
                  [src  "/scripts/smoke/picker.js"]))))

(define rollover-script
  (xml (script (@ [type "text/javascript"]
                  [src  "/scripts/smoke/rollover.js"]))))

(define excanvas-script
  (xml (!raw "<!--[if IE]>")
       (script (@ [type "text/javascript"]
                  [src  "/scripts/excanvas/excanvas.js"]))
       (!raw "<![endif]-->")))

(define firebug-script
  (xml (script (@ [type "text/javascript"]
                  [src  "/scripts/firebug/firebug-lite.js"]))))

(define default-tiny-mce-options-script
  (xml (script (@ [type "text/javascript"])
               (!raw "\n// <![CDATA[\n")
               (!raw ,(js (!dot tinyMCE (init (!object [mode  "none"]
                                                       [style "simple"])))))
               (!raw "\n// ]]>\n"))))

; jQuery UI --------------------------------------

(define jquery-ui-script/min
  (xml (script (@ [type "text/javascript"]
                  [src  "/scripts/jquery-ui/jquery-ui-1.7.1.custom.min.js"]))))

(define jquery-ui-script/dev
  (xml #;(script (@ [type "text/javascript"] [src "/scripts/jquery-ui/jquery-ui-1.7.2.custom.js"]))
       (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/dev/ui.core.js"]))
       (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/dev/ui.accordion.js"]))
       (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/dev/ui.datepicker.js"]))
       (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/dev/ui.draggable.js"]))
       (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/dev/ui.droppable.js"]))
       (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/dev/ui.resizable.js"]))
       (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/dev/ui.dialog.js"]))
       (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/dev/ui.progressbar.js"]))
       (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/dev/ui.selectable.js"]))
       (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/dev/ui.slider.js"]))
       (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/dev/ui.sortable.js"]))
       (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/dev/ui.tabs.js"]))
       (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/dev/effects.core.js"]))
       (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/dev/effects.blind.js"]))
       (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/dev/effects.bounce.js"]))
       (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/dev/effects.clip.js"]))
       (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/dev/effects.core.js"]))
       (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/dev/effects.drop.js"]))
       (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/dev/effects.explode.js"]))
       (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/dev/effects.fold.js"]))
       (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/dev/effects.highlight.js"]))
       (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/dev/effects.pulsate.js"]))
       (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/dev/effects.scale.js"]))
       (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/dev/effects.shake.js"]))
       (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/dev/effects.slide.js"]))
       (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/dev/effects.transfer.js"]))))

(define default-jquery-ui-styles
  (xml (link (@ [type "text/css"]
                [href "/styles/jquery-ui/ui-lightness/jquery-ui-1.7.2.custom.css"]
                [rel  "stylesheet"]))))

; Provide statements -----------------------------

(provide/contract
 [jquery-script/dev               xml?]
 [jquery-script/min               xml?]
 [smoke-script                    xml?]
 [smoke-styles                    xml?]
 [tooltip-script                  xml?]
 [picker-script                   xml?]
 [rollover-script                 xml?]
 [excanvas-script                 xml?]
 [firebug-script                  xml?]
 [jquery-ui-script/min            xml?]
 [jquery-ui-script/dev            xml?]
 [default-tiny-mce-options-script xml?]
 [default-jquery-ui-styles        xml?])
