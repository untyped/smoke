#lang scheme

(require (only-in srfi/13 string-pad)
         srfi/19
         (planet untyped/unlib:3/time)
         (only-in (planet untyped/unlib:3/list) list-ref?)
         "../../lib-base.ss"
         "regexp-field.ss")

(define-class time-field% regexp-field% ()   
  
  (inherit get-raw)
  
  (super-new [regexp        #px"^([0-9]{1,2}):([0-9]{2})$"]
             [format-string "HH:MM"]
             [max-length    5]
             [size          5])
  
  ; Public methods -----------------------------
  
  ; -> (U (cons integer integer) #f)
  (define/override (get-value)
    (let* ([val       (let ([raw (get-raw)]) (and raw (not (equal? raw "")) raw))]
           [time-vals (and val (regexp-match #px"^([0-9]{1,2}):([0-9]{2})$" val))]
           [hour      (and time-vals (list-ref? time-vals 1) (list-ref time-vals 1))]
           [mins      (and time-vals (list-ref? time-vals 2) (list-ref time-vals 2))])
      (cond [(not val) #f]
            [(or (not hour) (not mins))
             (raise-exn exn:smoke:form
               "Value must be in the format: HH:MM"
               this)]
            [else (cons (string->number hour) (string->number mins))])))
  
  ; (U (cons integer integer) #f) -> void
  (define/override (set-value! val)
    (let ([time (match val
                  [#f val]
                  [(cons (? (lambda (h) (and (integer? h) (<= 0 h) (< h 24))))
                         (? (lambda (m) (and (integer? m) (<= 0 m) (< m 60)))))
                   val]
                  [_ (raise-type-error 'set-value! "(U (cons hour minute) #f)" val)])])
      (super set-value! (and time (format "~a:~a"
                                          (string-pad (number->string (car time)) 2 #\0)
                                          (string-pad (number->string (cdr time)) 2 #\0)))))))

; Provide statements -----------------------------

(provide time-field%)
