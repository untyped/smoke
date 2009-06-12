#lang scheme/base

(require "../../lib-base.ss"
         "text-input.ss")

(define text-field%
  (class/cells text-input% ()
    
    (inherit core-html-attributes
             get-enabled?
             get-id
             get-raw
             set-raw!)
    
    ; Fields -------------------------------------
    
    ; (cell (U natural #f))
    (init-cell [size #f] #:accessor #:mutator)
    
    ; (cell (U natural #f))
    (init-cell [max-length #f] #:accessor #:mutator)
    
    ; (cell (U string #f))
    (init-cell [placeholder #f] #:accessor #:mutator)
    
    ; Constructor --------------------------------
    
    ; (listof symbol)
    (init [classes null])
    
    (super-new [classes (cons 'smoke-text-field classes)])
    
    ; Public methods -----------------------------
    
    ; seed -> xml
    (define/override (render seed)
      (define id          (get-id))
      (define size        (get-size))
      (define max-length  (get-max-length))
      (define raw         (get-raw))
      (xml (input (@ ,(core-html-attributes seed)
                     [type "text"]
                     ,(opt-xml-attr size)
                     ,(opt-xml-attr max-length maxlength max-length)
                     [value ,raw]))))
    
    ; request -> void
    (define/augride (on-request request)
      (when (get-enabled?)
        (let ([binding    (request-binding-ref request (get-id))]
              [max-length (get-max-length)])
          (when binding 
            (set-raw! (if (and max-length (< max-length (string-length binding)))
                          (substring binding 0 max-length)
                          binding))))))
    
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
    (define/augride (get-on-key-down seed)
      (js (if (== (!dot evt keyCode) 13)
              (= (!dot evt cancelBubble) #t))))
    
    ; seed -> js
    (define/augride (get-on-page-submit seed)
      (define selector (format "#~a" (get-id)))
      (js (var [fullRefresh (!index arguments 1)])
          (if (&& fullRefresh (!dot ($ ,selector) (hasClass "placeholder")))
              (!dot ($ ,selector) (val "")))))))

; Provide statements -----------------------------

(provide text-field%)
