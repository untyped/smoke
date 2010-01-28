#lang scheme/base

(require scheme/match
         scheme/pretty
         (planet untyped/unlib:3/list)
         (planet untyped/unlib:3/string)
         "../../lib-base.ss"
         "form-element.ss")

; To use this class, override:
;   - get-option     : -> (list-of any) ; return a list of Scheme items to show in the list
;   - option->raw    : any -> symbol    ; convert an item into a symbol to use as a form value
;   - raw->option    : symbol -> any    ; find an item given a symbol from the request bindings
;   - option->string : any -> string    ; convert an item into a user-friendly display string

; Vanilla Combo-box is basic elements; See below for more general radio-combo%
(define-class vanilla-radio-combo% form-element% ()
  
  (inherit get-id
           get-classes
           get-enabled?
           core-html-attributes)
  
  ; Fields -------------------------------------
  
  ; (cell (U string #f))
  ;
  ; "Raw" version of the selected value that can be embedded in XML output.
  ;
  ; This is converted to/from the "real" selected value using raw->item and item->raw.
  (cell raw-value #f #:accessor #:mutator)
  
  ; (cell boolean)
  (init-cell vertical? #f #:accessor #:mutator)
  
  ; Constructor --------------------------------
  
  (init [value   #f]
        [classes null])
  
  (when value (set-raw-value! (option->raw value)))
  
  (super-new [classes (cons 'smoke-radio-combo classes)])
  
  ; Public methods -----------------------------
  
  ; -> any
  (define/override (get-value)
    (raw->option (get-raw-value)))
  
  ; any -> void
  (define/override (set-value! value)
    (set-raw-value! (option->raw value)))
  
  ; -> boolean
  (define/override (value-valid?)
    #t)
  
  ; -> boolean
  (define/override (value-changed?)
    (web-cell-changed? raw-value-cell))
  
  ; -> (listof any)
  (define/public (get-options)
    (error "get-options must be overridden."))
  
  ; any -> (U string #f)
  (define/public (option->raw option)
    (error "option->raw must be overridden."))
  
  ; (U string #f) -> any
  (define/public (raw->option raw)
    (error "raw->option must be overridden."))
  
  ; any -> string
  (define/public (option->string option)
    (error "option->string must be overridden."))
  
  ; any -> string
  (define/public (option->xml option)
    (error "option->xml must be overridden."))
  
  ; seed -> xml
  (define/override (render seed)
    (let ([group-id  (get-id)]
          [raw-value (get-raw-value)]
          [options   (get-options)]
          [classes   (if (get-vertical?)
                         (cons 'radio-combo-vertical   (get-classes))
                         (cons 'radio-combo-horizontal (get-classes)))]
          [disabled  (and (not (get-enabled?)) "disabled")])
      (xml (div (@ ,(core-html-attributes seed #:classes classes))
                ,@(for/list ([option (get-options)])
                    (let* ([raw       (option->raw option)]
                           [button-id (format "~a-~a" group-id raw)]
                           [checked   (and (equal? raw raw-value) "checked")])
                      (xml (div (@ [class "radio-combo-item"])
                                (input (@ [type "radio"] [id ,button-id] [name ,group-id] [value ,raw] ,(opt-xml-attr checked) ,(opt-xml-attr disabled)))
                                (label (@ [for ,button-id]) ,(option->xml option))))))))))
  
  ; request -> void
  (define/augment (on-request request)
    (when (get-enabled?)
      (let ([binding (request-binding-ref request (get-id))])
        (when binding (set-raw-value! binding)))))
  
  ; seed -> js
  (define/augment (get-on-attach seed)
    (define group-id (get-id))
    (js ,@(for/list ([option (get-options)])
            (let* ([raw       (option->raw option)]
                   [button-id (format "~a-~a" group-id raw)]
                   [sel       (string-append "#" button-id)])
              (js (!dot ($ ,sel)
                        (click (function ()
                                 (!dot Smoke (setSubmitData
                                              ,group-id
                                              ,raw))))))))
        ,(inner (js) get-on-attach seed))))

(define-class radio-combo% vanilla-radio-combo% ()
  
  (inherit get-value
           set-value!)
  
  ; Fields -------------------------------------
  
  ; (cell (alistof (U boolean symbol number) (U string xml)))
  (init-cell options null)
  
  ; Constructor --------------------------------
  
  ; (U boolean symbol number void)
  ; The value void is used to mean "no value specified". In this case,
  ; the first option in options is supplied to the super-constructor.
  (init [value (void)])
  
  (super-new [value (if (void? value)
                        (and (not (null? options))
                             (caar options))
                        value)])
  
  ; Methods ------------------------------------
  
  ; -> (listof (U boolean symbol number))
  (define/override (get-options)
    (map car (web-cell-ref options-cell)))
  
  ; (alistof (U boolean symbol number) string) -> void
  (define/public (set-options! options)
    (web-cell-set! options-cell options)
    (unless (assq (get-value) options)
      (set-value! (and (not (null? options))
                       (caar options)))))
  
  ; (U bolean symbol number) -> string
  (define/override (option->raw option)
    (match option
      [(? number? num)   (number->string num)]
      [(? symbol? sym)   (symbol->string sym)]
      [(? boolean? bool) (if bool "--yes--" "--no--")]
      [other (raise-exn exn:fail:contract
               (format "Bad option key: expected (U boolean number symbol), received ~s" other))]))
  
  ; string -> (U boolean symbol number)
  (define/override (raw->option raw)
    (cond [(not raw)               #f]
          [(equal? raw "--yes--")  #t]
          [(equal? raw "--no--")   #f]
          [(string->number raw) => (lambda (num) num)]
          [else                    (string->symbol raw)]))
  
  ; (U boolean symbol number) ->  string
  (define/override (option->string option)
    (let ([option-string (assoc-value/default option (web-cell-ref options-cell) #f)])
      (cond [(string? option-string) option-string]
            [(xml? option-string)    (xml->string option-string)]
            [else (error (format "Not a valid option: ~s ~s" (web-cell-ref options-cell) option))])))
  
  ; (U boolean symbol number) ->  xml
  (define/override (option->xml option)
    (let ([option-xml (assoc-value/default option (web-cell-ref options-cell) #f)])
      (cond [(string? option-xml) (xml ,option-xml)]
            [(xml? option-xml)    option-xml]
            [else (error (format "Not a valid option: ~s ~s" (web-cell-ref options-cell) option))]))))

; Provide statements -----------------------------

(provide vanilla-radio-combo%
         radio-combo%)
