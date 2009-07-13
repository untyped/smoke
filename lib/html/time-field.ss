#lang scheme/base

(require srfi/19
         (planet untyped/unlib:3/time)
         (only-in (planet untyped/unlib:3/list) list-ref?)
         "../../lib-base.ss"
         "regexp-field.ss")

(define time-field%
  (class/cells regexp-field% ()    
    
    (super-new [regexp        "^[0-9]{1,2}:[0-9]{2}$"]
               [format-string "HH:MM"]
               [max-length    5]
               [size          5])
    
    ; Public methods -----------------------------
    
    ; -> (U (cons integer integer) #f)
    (define/override (get-value)
      (let*/debug ([val       (super get-value)]
                   [time-vals (and val (regexp-match #px"^([0-9]{1,2}):([0-9]{2})$"))]
                   [hour      (and time-vals (list-ref? time-vals 1) (list-ref time-vals 1))]
                   [mins      (and time-vals (list-ref? time-vals 2) (list-ref time-vals 2))])
                  
                  (if (and val (or (not hour) (not mins)))
                      (raise-exn exn:smoke:form (format "time value must be in the format: HH:MM") this)
                      (cons hour mins))))
    
    ; (U (cons integer integer) #f) -> void
    (define/override (set-value! val)
      (let ([time (match val
                    [#f                      val]
                    [(cons integer? integer) val]
                    [_                       (raise-type-error 'set-value! "(U (cons hour minute) #f)" val)])])
        (super set-value! (and time (format "~a:~a"
                                            (car time) 
                                            (string-pad (number->string (cdr time)) 2 #\0))))))))

; Provide statements -----------------------------

(provide time-field%)
