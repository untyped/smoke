#lang scheme/base

(require "../../lib-base.ss"
         "text-input.ss")

(define placeholder<%>
  (interface ()
    get-placeholder    ; -> string
    set-placeholder!)) ; string -> void

(define placeholder-mixin
  (mixin/cells (text-input<%>) (placeholder<%>)
    
    (inherit get-enabled?
             get-id
             get-raw
             set-raw!)
    
    ; Fields -------------------------------------
    
    ; (cell (U string #f))
    (init-cell placeholder #f #:accessor #:mutator)
    
    ; Constructor --------------------------------
 
    ; seed -> js
    (define/augment (get-on-attach seed)
      (define raw         (get-raw))
      (define placeholder (get-placeholder))
      (js ,(opt-js (and placeholder (string=? raw ""))
                   (!dot ($ ,(format "#~a" (get-id)))
                         (addClass "placeholder")
                         (val ,placeholder)))
          ,(inner (js) get-on-attach seed)))
    
    ; seed -> js
    (define/augride (get-on-focus seed)
      (define selector    (format "#~a" (get-id)))
      (define raw         (get-raw))
      (define placeholder (get-placeholder))
      (opt-js placeholder
              (if (!dot ($ ,selector)
                        (hasClass "placeholder"))
                  (!dot ($ ,selector) 
                        (removeClass "placeholder")
                        (val "")))))
    
    ; seed -> js
    (define/augride (get-on-blur seed)
      (define selector    (format "#~a" (get-id)))
      (define raw         (get-raw))
      (define placeholder (get-placeholder))
      (opt-js placeholder
              (if (== (!dot ($ ,selector) (val)) "")
                  (!block (!dot ($ ,selector) 
                                (addClass "placeholder")
                                (val ,placeholder))))))
    
    ; seed -> js
    (define/augride (get-on-page-submit seed)
      (define selector (format "#~a" (get-id)))
      (js (var [fullRefresh (!index arguments 1)])
          (if (&& fullRefresh (!dot ($ ,selector) (hasClass "placeholder")))
              (!dot ($ ,selector) (val "")))))))

; Provide statements -----------------------------

(provide placeholder<%> placeholder-mixin)
