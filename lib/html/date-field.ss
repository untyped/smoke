#lang scheme/base

(require srfi/19
         "../../lib-base.ss"
         "text-field.ss")

(define date-field%
  (class/cells text-field% ()    
    
    ; Fields -------------------------------------    
    
    ; (cell (U string #f))
    (init-cell [date-string #f]
               #:accessor #:mutator)
    ; (cell (U string #f))
    (init-cell [pretty-date-string #f]
               #:accessor #:mutator)
    ; (cell boolean)
    (init-cell [show-date-label? #f]
               #:accessor #:mutator)
    
    ; Public methods -----------------------------
    
    ; date -> void
    (define/override (set-value! val)
      (super set-value! (and val (date->string val (get-date-string)))))    
    
    ; -> (U date #f)
    (define/override (get-value)
      (define val (super get-value))
      (cond [(not val)                      #f]
            [(not (get-date-string))        (error "date-string is #f")]
            [(not (get-pretty-date-string)) (error "pretty-date-string is #f")]
            [else                      
             (with-handlers ([exn:fail? 
                              (lambda (exn)
                                (raise-exn exn:smoke:form 
                                           (format "This value must be in the format: ~a" (get-pretty-date-string))
                                           this))])
               (string->date val (get-date-string)))]))
    
    ; seed -> xml
    (define/override (render seed)
      (xml ,(super render seed)
           ,(opt-xml (and (get-show-date-label?) (get-pretty-date-string))
              ,(get-pretty-date-string))))))

; Provide statements -----------------------------

(provide date-field%)
