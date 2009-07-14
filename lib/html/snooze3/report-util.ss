#lang scheme/base

(require "../../../lib-base.ss")

(require srfi/13
         srfi/19
         (planet untyped/unlib:3/time))

; Search patterns --------------------------------

; string [boolean] -> string
(define (pattern->regexp pattern [anywhere? #f])
  (apply string-append (cons (if anywhere? "^.*" "^")
                             (string-fold-right (lambda (chr accum)
                                                  (cond [(eq? chr #\*) (cons ".*" accum)]
                                                        [(eq? chr #\?) (cons "." accum)]
                                                        [else          (cons (regexp-quote (string chr)) accum)]))
                                                null
                                                pattern))))

; string -> (U time-utc #f)
(define (pattern->time pattern)
  
  ; -> time-utc
  (define (today)
    (let ([now (current-date)])
      (date->time-utc
       (copy-date now 
                  #:nanosecond  0
                  #:second      0
                  #:minute      0
                  #:hour        0
                  #:zone-offset 0))))
  
  ; time -> time-utc
  (define (day-before time)
    (subtract-duration time (make-time time-duration 0 (* 60 60 24))))
  
  ; symbol -> time-utc
  (define (last day-of-the-week)
    (let loop ([now (day-before (today))])
      (if (equal? (date-day-of-the-week (time->date now)) day-of-the-week)
          now
          (loop (day-before now)))))
  
  ; (U time-utc #f)
  (let ([pattern (string-downcase pattern)])
    (with-handlers ([exn? (lambda (exn) #f)])
      (cond [(equal? pattern "today")              (today)]
            [(equal? pattern "yesterday")          (day-before (today))]
            [(member pattern '("mon" "monday"))    (last 'mon)]
            [(member pattern '("tue" "tuesday"))   (last 'tue)]
            [(member pattern '("wed" "wednesday")) (last 'wed)]
            [(member pattern '("thu" "thursday"))  (last 'thu)]
            [(member pattern '("fri" "friday"))    (last 'fri)]
            [(member pattern '("sat" "saturday"))  (last 'sat)]
            [(member pattern '("sun" "sunday"))    (last 'sun)]
            [(regexp-match #rx"([0-9][0-9]?)/([0-9][0-9]?)" pattern)
             => (match-lambda 
                  [(list _ day month)
                   (let* ([now  (time->date (today))]
                          [then (copy-date now
                                           #:day    (string->number day) 
                                           #:month  (string->number month))]
                          [ans  (if (time>? (date->time-utc then)
                                            (date->time-utc now))
                                    (copy-date then #:year (sub1 (date-year then)))
                                    then)])
                     (and (date-valid? ans) (date->time-utc ans)))])]
            [(regexp-match #rx"([0-9][0-9]?):([0-9][0-9]?)" pattern)
             => (match-lambda 
                  [(list _ hour min)
                   (let* ([now (time->date (today))]
                          [ans (copy-date now
                                          #:hour   (string->number hour)
                                          #:minute (string->number min))])
                     (and (date-valid? ans) (date->time-utc ans)))])]
            [else #f]))))

; Provide statements -----------------------------

(provide/contract
 [pattern->regexp (->* (string?) (boolean?) string?)]
 [pattern->time   (-> string? (or/c time-utc? #f))])