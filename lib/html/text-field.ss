#lang web-server

(require "../../lib-base.ss"
         "placeholder.ss"
         "text-input.ss")

(define text-field%
  (class/cells (placeholder-mixin text-input%) ()
    
    (inherit core-html-attributes
             get-enabled?
             get-id
             get-raw
             set-raw!)
    
    ; Fields -------------------------------------
    
    ; (cell (U natural #f))
    (init-cell size #f #:accessor #:mutator)
    
    ; (cell (U natural #f))
    (init-cell max-length #f #:accessor #:mutator)
    
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
    (define/augride (get-on-key-down seed)
      (js (if (== (!dot evt keyCode) 13)
              (!dot evt (stopPropagation)))))))

; Provide statements -----------------------------

(provide text-field%)
