#lang web-server

(require "../../lib-base.ss"
         "text-input.ss")

(define password-field%
  (class/cells text-input% ()
    
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
    
    (super-new [classes (cons 'smoke-password-field classes)])
    
    ; Public methods -----------------------------
    
    ; seed -> xml
    (define/override (render seed)
      (define id         (get-id))
      (define size       (get-size))
      (define max-length (get-max-length))
      (define raw        (get-raw))
      (xml (input (@ ,(core-html-attributes seed)
                     [type "password"]
                     ,(opt-xml-attr size)
                     ,(opt-xml-attr max-length maxlength max-length)))))    
    
    ; seed -> js
    (define/augride (get-on-key-down seed)
      (js (if (== (!dot evt keyCode) 13) ; remove "enter" behaviour
              (!dot evt (preventDefault)))))
    
    ; request -> void
    (define/augride (on-request request)
      (when (get-enabled?)
        (let ([binding    (request-binding-ref request (get-id))]
              [max-length (get-max-length)])
          (when binding 
            (set-raw! (if (and max-length (< max-length (string-length binding)))
                          (substring binding 0 max-length)
                          binding))))))))

; Provide statements -----------------------------

(provide password-field%)
