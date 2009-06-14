#lang scheme/base

(require srfi/19
         (planet untyped/unlib:3/time)
         "../../lib-base.ss"
         "browser-util.ss"
         "form-element.ss"
         "text-field.ss")

(define date-field<%>
  (interface (form-element<%>)
    get-date
    get-time-utc
    get-time-tai))

(define date-field%
  (class/cells text-field% (date-field<%>)    
    
    (inherit get-id)
    
    ; Fields -------------------------------------    
    
    ; (cell string)
    (init-cell date-format
      "~Y-~m-~d ~H:~M"
      #:accessor #:mutator)
    
    ; (cell boolean)
    (init-cell show-date-label? #f
      #:accessor #:mutator)
    
    ; (cell (U string #f))
    (init-cell date-picker-format
      (date-format->jquery-ui-date-format date-format)
      #:accessor #:mutator)
    
    ; Constructor --------------------------------
    
    (init [classes     null]
          [max-length  (date-format->max-length date-format)]
          [size        max-length]
          [placeholder (get-date-format-example)])
    
    (super-new [classes     (list* 'smoke-date-field classes)]
               [size        size]
               [max-length  max-length]
               [placeholder placeholder])
    
    ; Methods ------------------------------------
    
    ; -> (listof (U xml (seed -> xml)))
    (define/augment (get-html-requirements)
      (if (get-date-picker-format)
          (list* jquery-ui-script (inner null get-html-requirements))
          (inner null get-html-requirements)))
    
    ; -> string
    (define/public (get-date-format-example)
      (date->string (current-date) (get-date-format)))
    
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
          (and val (string->date val fmt)))))
    
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
              " example: " ,(get-date-format-example))))
    
    ; seed -> js
    (define/augment (get-on-attach seed)
      (let ([fmt (get-date-picker-format)])
        (js ,(opt-js fmt
               (!dot ($ ,(format "#~a" (get-id)))
                     (datepicker (!object [dateFormat      ,fmt]
                                          [showOn          "button"]
                                          [buttonImage     "/images/jquery-ui/calendar.gif"]
                                          [buttonImageOnly #t]))))
            ,(inner (js) get-on-attach seed))))
    
    ; seed -> js
    (define/augment (get-on-detach seed)
      (let ([fmt (get-date-picker-format)])
        (js ,(opt-js fmt
               (!dot ($ ,(format "#~a" (get-id)))
                     (datepicker "destroy")))
            ,(inner (js) get-on-detach seed))))))

; Helpers ----------------------------------------

; (U string #f) -> (U natural #f)
(define (date-format->max-length fmt)
  (and fmt (for/fold ([accum (string-length fmt)])
                     ([card (in-list (regexp-match* #px"~." fmt))])
                     (+ accum (match card
                                ["~~" -1]
                                ["~a" 2]
                                ["~A" 8]
                                ["~b" 2]
                                ["~B" 8]
                                ["~d" 1]
                                ["~e" 1]
                                ["~h" 1]
                                ["~H" 1]
                                ["~k" 1]
                                ["~m" 1]
                                ["~M" 1]
                                ["~S" 1]
                                ["~y" 1]
                                ["~Y" 3]
                                ["~z" 4]
                                [_    1])))))

; (U string #f) -> (U natural #f)
(define (date-format->size fmt)
  (and fmt (date-format->max-length fmt)))

; string -> (U string #f)
(define (date-format->jquery-ui-date-format fmt)
  (let/ec return
    (regexp-replace*
     #px"~."
     (regexp-replace*
      #px"(~.)?([^~]+)(~.)?"
      (regexp-replace* #px"'" fmt "''")
      (lambda (a b c d)
        (if b
            (if d
                (format "~a'~a'~a" b c d)
                (format "~a'~a'" b c))
            (if d
                (format "'~a'~a" c d)
                (format "'~a'" c)))))
     (match-lambda
       ["~~" "~"]
       ["~a" "D"]
       ["~A" "DD"]
       ["~b" "M"]
       ["~B" "MM"]
       ["~d" "dd"]
       ["~e" "d"]
       ["~h" "M"]
       #;["~H" (return #f)]
       #;["~k" (return #f)]
       ["~m" "mm"]
       #;["~M" (return #f)]
       #;["~S" (return #f)]
       ["~y" "y"]
       ["~Y" "yy"]
       [_ (return #f)]))))

; Provide statements -----------------------------

(provide date-field<%>
         date-field%)