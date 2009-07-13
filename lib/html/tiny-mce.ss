#lang scheme/base

(require "../../lib-base.ss"
         "browser-util.ss"
         "html-element.ss"
         "html-page.ss"
         "text-area.ss")

; Interfaces -------------------------------------

(define tiny-mce-options<%>
  (interface (html-page<%>)
    get-tiny-mce-options)) ; seed -> javascript-object

; Mixins -----------------------------------------

(define tiny-mce-options-mixin
  (mixin/cells (html-page<%> html-element<%>) (tiny-mce-options<%>)
    
    ; Constructor --------------------------------
    
    (super-new)
    
    ; Methods ------------------------------------
    
    ; seed -> xml
    ;
    ; This has to be a (define ...) rather than a (define/foo ...).
    ;
    ; We want make-tiny-mce-options to be a simple statically bound procedure
    ; rather than a method, because we want it to remain constant across calls
    ; to get-html-requirements.
    (define (make-tiny-mce-options-script seed)
      (xml (script (@ [type "text/javascript"])
                   (!raw "\n// <![CDATA[\n")
                   (!raw ,(js ((function ()
                                 (var [tinyMCEOptions ,(send this get-tiny-mce-options seed)])
                                 (= (!dot tinyMCEOptions mode) "none")
                                 (!dot tinyMCE (init tinyMCEOptions))))))
                   (!raw "\n// ]]>\n"))))
    
    ; -> (listof (U xml (seed -> xml)))
    (define/augment (get-html-requirements)
      (list* tiny-mce-script
             make-tiny-mce-options-script
             (inner null get-html-requirements)))
    
    ; seed -> javascript
    (define/public (get-tiny-mce-options seed)
      (js (!object [mode "none"])))))

; Classes ----------------------------------------

(define tiny-mce%
  (class/cells simple-text-area% ()
    
    (inherit get-id)
    
    ; Constructor --------------------------------
    
    ; (listof symbol)
    (init [classes null])
    (super-new [classes (cons 'smoke-tiny-mce classes)])
    
    ; Public methods -----------------------------
    
    ; -> (listof (U xml (seed -> xml)))
    (define/augment (get-html-requirements)
      (if (is-a? (current-page) tiny-mce-options<%>)
          (list* tiny-mce-script
                 (inner null get-html-requirements))
          (list* tiny-mce-script
                 default-tiny-mce-options-script
                 (inner null get-html-requirements))))
    
    ; seed -> javascript
    (define/augment (get-on-attach seed)
      (let ([id (get-id)])
        (js (!dot tinyMCE (execCommand "mceAddControl" #f ,id)))))
    
    ; seed -> javascript
    (define/augment (get-on-detach seed)
      (let ([id (get-id)])
        (js (!dot tinyMCE (execCommand "mceRemoveControl" #f ,id)))))
    
    ; seed -> (U javascript #f)
    ;
    ; Don't use the on-change event from text-area%.
    (define/override (get-on-change seed)
      #f)))

; Helpers ----------------------------------------

; xml
(define tiny-mce-script
  (xml (script (@ [type "text/javascript"]
                  [src  "/scripts/tiny_mce/tiny_mce.js"]))
       (script (@ [type "text/javascript"])
               (!raw "\n// <![CDATA[\n")
               (!raw ,(js (!dot ($ document)
                                (bind "smoke-page-submit"
                                      (function (evt fullRefresh)
                                        (!dot ($ ".smoke-tiny-mce")
                                              (each (function ()
                                                      (var [id (!dot this id)])
                                                      (!dot Smoke (setSubmitData id (!dot tinyMCE (get id) (getContent))))))))))))
               (!raw "\n// ]]>\n"))))

; Provide statements -----------------------------

(provide tiny-mce-options<%>
         tiny-mce-options-mixin
         tiny-mce%)
