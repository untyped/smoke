#lang scheme/base

(require "../../lib-base.ss")

(require (planet untyped/unlib:3/enumeration))

; XML requirements -------------------------------

; jQuery -----------------------------------------

(define-enum jquery-versions
  (1.3.2 1.4.2))

; [jquery-version] -> xml
(define (jquery-script/min [version (jquery-versions 1.3.2)])
  (xml (script (@ [type "text/javascript"]
                  [src  ,(format "/scripts/jquery/~a/jquery.min.js" version)]))))

; [jquery-version] -> xml
(define (jquery-script/dev [version (jquery-versions 1.3.2)])
  (xml (script (@ [type "text/javascript"]
                  [src  ,(format "/scripts/jquery/~a/jquery.js" version)]))))

; jQuery UI --------------------------------------

(define-enum jquery-ui-versions
  (1.7.1 1.8.0))

; [jquery-ui-version] -> xml
(define (jquery-ui-script/min [version (jquery-ui-versions 1.7.1)])
  (xml (script (@ [type "text/javascript"]
                  [src  ,(format "/scripts/jquery-ui/~a/jquery-ui.min.js" version)]))))

; [jquery-ui-version] -> xml
(define (jquery-ui-script/dev [version (jquery-ui-versions 1.7.1)])
  (enum-case jquery-ui-versions version
    [(1.7.1) (xml (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.7.1/jquery.ui.core.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.7.1/jquery.ui.accordion.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.7.1/jquery.ui.datepicker.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.7.1/jquery.ui.draggable.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.7.1/jquery.ui.droppable.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.7.1/jquery.ui.resizable.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.7.1/jquery.ui.dialog.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.7.1/jquery.ui.progressbar.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.7.1/jquery.ui.selectable.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.7.1/jquery.ui.slider.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.7.1/jquery.ui.sortable.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.7.1/jquery.ui.tabs.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.7.1/jquery.effects.core.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.7.1/jquery.effects.blind.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.7.1/jquery.effects.bounce.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.7.1/jquery.effects.clip.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.7.1/jquery.effects.drop.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.7.1/jquery.effects.explode.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.7.1/jquery.effects.fold.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.7.1/jquery.effects.highlight.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.7.1/jquery.effects.pulsate.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.7.1/jquery.effects.scale.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.7.1/jquery.effects.shake.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.7.1/jquery.effects.slide.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.7.1/jquery.effects.transfer.js"])))]
    [(1.8.0) (xml (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.8.0/jquery.ui.core.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.8.0/jquery.ui.widget.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.8.0/jquery.ui.mouse.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.8.0/jquery.ui.position.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.8.0/jquery.ui.draggable.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.8.0/jquery.ui.droppable.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.8.0/jquery.ui.resizable.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.8.0/jquery.ui.selectable.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.8.0/jquery.ui.sortable.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.8.0/jquery.ui.accordion.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.8.0/jquery.ui.autocomplete.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.8.0/jquery.ui.menu.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.8.0/jquery.ui.button.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.8.0/jquery.ui.dialog.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.8.0/jquery.ui.slider.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.8.0/jquery.ui.tabs.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.8.0/jquery.ui.datepicker.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.8.0/jquery.ui.progressbar.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.8.0/jquery.effects.core.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.8.0/jquery.effects.blind.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.8.0/jquery.effects.bounce.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.8.0/jquery.effects.clip.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.8.0/jquery.effects.drop.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.8.0/jquery.effects.explode.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.8.0/jquery.effects.fold.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.8.0/jquery.effects.highlight.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.8.0/jquery.effects.pulsate.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.8.0/jquery.effects.scale.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.8.0/jquery.effects.shake.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.8.0/jquery.effects.slide.js"]))
                  (script (@ [type "text/javascript"] [src "/scripts/jquery-ui/1.8.0/jquery.effects.transfer.js"])))]))

; [jquery-ui-version] [string] -> xml
(define (jquery-ui-styles [version (jquery-ui-versions 1.7.1)] [theme "ui-lightness"])
  (xml (link (@ [rel   "stylesheet"] 
                [href  ,(format "/styles/jquery-ui/~a/~a/jquery-ui.css" version theme)]
                [type  "text/css"]
                [media "screen, projection"]))))

; Smoke ------------------------------------------

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

; Smoke "plugins" --------------------------------

; ("Plugin" isn't an official term!)

(define tooltip-script
  (xml (script (@ [type "text/javascript"]
                  [src  "/scripts/smoke/ui.stackfix.js"]))
       (script (@ [type "text/javascript"]
                  [src  "/scripts/smoke/ui.position.js"]))
       (script (@ [type "text/javascript"]
                  [src  "/scripts/smoke/tooltip.js"]))))

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

; Provide statements -----------------------------

(provide jquery-versions
         jquery-ui-versions)

(provide/contract
 [jquery-script/dev               (->* () ((enum-value/c jquery-versions)) xml?)]
 [jquery-script/min               (->* () ((enum-value/c jquery-versions)) xml?)]
 [jquery-ui-script/min            (->* () ((enum-value/c jquery-ui-versions)) xml?)]
 [jquery-ui-script/dev            (->* () ((enum-value/c jquery-ui-versions)) xml?)]
 [jquery-ui-styles                (->* () ((enum-value/c jquery-ui-versions) string?) xml?)]
 [default-tiny-mce-options-script xml?]
 [smoke-script                    xml?]
 [smoke-styles                    xml?]
 [tooltip-script                  xml?]
 [rollover-script                 xml?]
 [excanvas-script                 xml?]
 [firebug-script                  xml?])
