#lang web-server

(require "../../lib-base.ss"
         "html-element.ss")

(define html-dialog%
  (class/cells html-element% ()
    
    (inherit core-html-attributes
             get-id)
    
    ; Fields -------------------------------------
    
    ; (U html-page% #f)
    (cell page #f #:accessor #:mutator)
    
    ; Constructor --------------------------------
    
    ; (listof symbol)
    (init [classes null])
    
    (super-new [classes (cons 'smoke-dialog classes)])
    
    ; Methods ------------------------------------
    
    ; -> (listof (U js (seed -> js)))
    (define/augment (get-js-requirements)
      (list* html-dialog-script
             (inner null get-js-requirements)))
    
    ; seed -> xml
    (define/overment (render seed)
      (xml (div (@ [class "smoke-dialog-background"])
                (div (@ ,@(core-html-attributes seed))
                     ,(inner (xml "Insert dialog content here.") render seed)))))
    
    ; seed -> js
    (define/augride (get-on-attach seed)
      (js (!dot Smoke (showDialog Smoke (findById ,(get-id))))))))

; Javascript resources ---------------------------

; js
(define html-dialog-script
  (js (= (!dot Smoke showDialog)
         (function (frame)
           (var [bg       (!dot frame parentNode)]
                [recenter (function ()
                            (var [viewSize  (!dot Smoke (getViewportDimensions))]
                                 [frameSize (!dot frame (getDimensions))])
                            (!dot frame (setStyle (!object ["top"  (+ (- (/ (!dot viewSize height) 2)
                                                                         (/ (!dot frameSize height) 2))
                                                                      "px")]
                                                           ["left" (+ (- (/ (!dot viewSize width) 2)
                                                                         (/ (!dot frameSize width) 2))
                                                                      "px")]))))]
                [style (!object [display "block"])])
           (!dot ($ window) (bind "resize" recenter))
           (recenter)
           (!dot window (setTimeout (function ()
                                      (!dot bg (setStyle style))
                                      (!dot window (setTimeout (function () 
                                                                 (!dot frame (setStyle style))
                                                                 (recenter))
                                                               0)))
                                    0))))))

; Provide statements -----------------------------

(provide html-dialog%)
