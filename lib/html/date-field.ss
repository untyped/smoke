#lang scheme/base

(require srfi/19
         (planet untyped/unlib:3/time)
         "../../lib-base.ss"
         "form-element.ss"
         "text-field.ss")

(define date-field<%>
  (interface (form-element<%>)
    get-date
    get-time-utc
    get-time-tai))

(define date-field%
  (class/cells text-field% (date-field<%>)    
    
    ; Fields -------------------------------------    
    
    ; (cell string)
    (init-cell date-format "~Y-~m-~d ~H:~M" #:accessor #:mutator)
    
    ; (cell boolean)
    (init-cell show-date-label? #f #:accessor #:mutator)
    
    ; Public methods -----------------------------
    
    ; -> string
    (define/public (get-date-format-example)
      (date-format-string->example (get-date-format)))
    
    ; -> (U date #f)
    (define/override (get-value)
      (get-date))
    
    ; -> (U date #f)
    (define/public (get-date)
      (let ([val (super get-value)]
            [fmt (get-date-format)])
        (with-handlers ([exn:fail? (lambda (exn)
                                     (raise-exn exn:smoke:form 
                                       (format "value must be in the format: ~a" (get-date-format-example))
                                       this))])
          (string->date val fmt))))
    
    ; -> (U time-utc #f)
    (define/public (get-time-utc)
      (let ([val (get-date)])
        (and val (date->time-utc val))))
    
    ; -> (U time-tai #f)
    (define/public (get-time-tai)
      (let ([val (get-date)])
        (and val (date->time-tai val))))
    
    ; date -> void
    (define/override (set-value! val)
      (let ([date (cond [(not val)       val]
                        [(date? val)     val]
                        [(time-tai? val) (time-tai->date val)]
                        [(time-utc? val) (time-utc->date val)]
                        [else            (raise-type-error 'set-value! "(U date time-utc time-tai #f)" val)])])
        (super set-value! (and date (date->string date (get-date-format))))))
    
    ; seed -> xml
    (define/override (render seed)
      (xml ,(super render seed)
           ,(opt-xml (get-show-date-label?)
              " example: " ,(get-date-format-example))))))

; Helpers ----------------------------------------

(define (date-format-string->example str)
  (let ([now (current-date)])
    (for/fold ([str str])
              ([wildcard (in-list (regexp-match* #px"(~[a-zA-Z])" str))])
              (regexp-replace* wildcard str (date->string now wildcard)))))

; Provide statements -----------------------------

(provide date-field<%>
         date-field%)
