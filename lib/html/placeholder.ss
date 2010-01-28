#lang scheme/base

(require "../../lib-base.ss"
         "text-input.ss")

(define placeholder<%>
  (interface ()
    get-placeholder    ; -> string
    set-placeholder!)) ; string -> void

(define-mixin placeholder-mixin (text-input<%>) (placeholder<%>)
  
  (inherit get-enabled?
           get-id
           get-raw
           set-raw!)
  
  ; Fields -------------------------------------
  
  ; (cell (U string #f))
  (init-cell placeholder #f #:accessor #:mutator)
  
  ; Constructor --------------------------------
  
  ; -> (listof (U xml (seed -> xml)))
  (define/augment (get-html-requirements)
    (list* placeholder-script
           (inner null get-html-requirements)))
  
  ; seed -> js
  (define/augment (get-on-attach seed)
    (define raw         (get-raw))
    (define placeholder (get-placeholder))
    (js ,(opt-js (and placeholder (string=? raw ""))
           (!dot ($ ,(format "#~a" (get-id)))
                 (addClass "smoke-placeholder")
                 (val ,placeholder)))
        ,(inner (js) get-on-attach seed)))
  
  ; seed -> js
  (define/augride (get-on-focus seed)
    (define selector    (format "#~a" (get-id)))
    (define raw         (get-raw))
    (define placeholder (get-placeholder))
    (opt-js placeholder
      (if (!dot ($ ,selector)
                (hasClass "smoke-placeholder"))
          (!dot ($ ,selector) 
                (removeClass "smoke-placeholder")
                (val "")))))
  
  ; seed -> js
  (define/augride (get-on-blur seed)
    (define selector    (format "#~a" (get-id)))
    (define raw         (get-raw))
    (define placeholder (get-placeholder))
    (opt-js placeholder
      (if (== (!dot ($ ,selector) (val)) "")
          (!block (!dot ($ ,selector) 
                        (addClass "smoke-placeholder")
                        (val ,placeholder)))))))

; Helpers ----------------------------------------

; xml
(define placeholder-script
  (xml (script (@ [type "text/javascript"])
               (!raw "\n// <![CDATA[\n")
               (!raw ,(js (!dot ($ document)
                                (bind "smoke-page-submit"
                                      (function (evt fullRefresh)
                                        (if fullRefresh
                                            (!dot ($ ".smoke-placeholder") (val ""))))))))
               (!raw "\n// ]]>\n"))))

; Provide statements -----------------------------

(provide placeholder<%> placeholder-mixin)
