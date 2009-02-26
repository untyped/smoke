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
      (xml (!raw "\n// <![CDATA[\n")
           (!raw ,(js ((function ()
                         (var [tinyMCEOptions ,(send this get-tiny-mce-options seed)])
                         (= (!dot tinyMCEOptions mode) "none")
                         (!dot tinyMCE (init tinyMCEOptions))))))
           (!raw "\n// ]]>\n")))
    
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
  (class/cells text-area% ()
    
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
        (js #;(if (! (!dot Smoke tinyMCECache))
                (= (!dot Smoke tinyMCECache)
                   (!object)))
            
            (!dot tinyMCE (execCommand "mceAddControl" #f ,id))
            
            ; There's no reliable way of getting hold of the tinyMCE instance
            ; yet to register an onInit event, so we resort to this rather
            ; clumsy event loop:
            
            #;(var [tries       0])
            #;(var [saveContent (function ()
                                  (if (> tries 20)
                                      (!block (alert "Warning: the rich text editor did not initialise correctly."))
                                      (!block (if (&& (!dot tinyMCE (get ,id))
                                                      (!dot tinyMCE (get ,id) (getContent)))
                                                  (= (!dot Smoke (!index tinyMCECache ,id))
                                                     (!dot tinyMCE (get ,id) (getContent)))
                                                  (!dot window (setTimeout saveContent 100)))
                                              (= tries (+ tries 1)))))])
            
            #;(saveContent))))
    
    ; seed -> javascript
    (define/augment (get-on-detach seed)
      (let ([id (get-id)])
        (js (!dot tinyMCE (execCommand "mceRemoveControl" #f ,id)))))
    
    ; seed -> javascript
    (define/augment (get-on-page-submit seed)
      (let ([id (get-id)])
        (js
         ;(if (!dot Smoke tinyMCECache)
         ;    (!block (var [oldContent (!dot Smoke (!index tinyMCECache ,id))]
         ;                 [newContent (!dot tinyMCE (get ,id) (getContent))])
         ;            (if (!= oldContent newContent)
         ;                 (!dot Smoke (setSubmitData ,(get-id) (!dot tinyMCE (get ,(get-id)) (getContent)))))))
         (!dot Smoke (setSubmitData ,(get-id) (!dot tinyMCE (get ,(get-id)) (getContent)))))))
    
    ; seed -> (U javascript #f)
    ;
    ; Don't use the on-change event from text-area%.
    (define/override (get-on-change seed)
      #f)))

; Provide statements -----------------------------

(provide tiny-mce-options<%>
         tiny-mce-options-mixin
         tiny-mce%)
