#lang scheme/base

(require scheme/match
         scheme/pretty
         (only-in scheme/string string-join)
         (planet untyped/unlib:3/list)
         (planet untyped/unlib:3/string)
         "../../lib-base.ss"
         "form-element.ss")

; To use this class, override:
;   - get-option     : -> (list-of any) ; return a list of Scheme items to show in the list
;   - option->raw    : any -> symbol    ; convert an item into a symbol to use as a form value
;   - raw->option    : symbol -> any    ; find an item given a symbol from the request bindings
;   - option->string : any -> string    ; convert an item into a user-friendly display string

; Vanilla Combo-box is basic elements; See below for more general combo-box%
(define vanilla-combo-box%
  (class/cells form-element% ()
    
    (inherit get-id
             get-enabled?
             core-html-attributes)
    
    ; Fields -------------------------------------
    
    ; (cell (U string #f))
    ;
    ; "Raw" version of the selected value that can be embedded in XML output.
    ;
    ; This is converted to/from the "real" selected value using raw->item and item->raw.
    (cell raw-value #f #:accessor #:mutator)
    
    ; Constructor --------------------------------
    
    (init [value #f])
    (init [classes null])
    
    (when value (set-raw-value! (option->raw value)))
    (super-new [classes (cons 'smoke-combo-box classes)])
    
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
    
    ; any -> (listof string)
    (define/public (option->classes option)
      null)
    
    ; any -> (U string symbol #f)
    (define/public (option->id option)
      #f)
    
    ; seed -> xml
    (define/override (render seed)
      (let ([id        (get-id)]
            [raw-value (get-raw-value)]
            [options   (get-options)])
        (xml (select (@ ,(core-html-attributes seed))
                     ,@(for/list ([option (in-list options)])
                         (let* ([raw-option (option->raw option)]
                                [selected   (and (equal? raw-option raw-value) "selected")]
                                [classes    (option->classes option)]
                                [id         (option->id option)]
                                [class      (and classes (string-join (map string+symbol->string classes) " "))])
                           (xml (option (@ [value ,raw-option] ,(opt-xml-attr id) ,(opt-xml-attr class) ,(opt-xml-attr selected))
                                        ,(option->string option)))))))))
    
    ; request -> void
    (define/augment (on-request request)
      (when (get-enabled?)
        (let ([binding (request-binding-ref request (get-id))])
          (when binding (set-raw-value! binding))
          (inner (void) on-request request))))
    
    ; seed -> js
    (define/augment (get-on-change seed)
      (define id (get-id))
      (js (!dot Smoke (setSubmitData ,id (!dot Smoke (findById ,id) value)))
          ,(inner (js) get-on-change seed)))))

(define combo-box%
  (class/cells vanilla-combo-box% ()
    
    (inherit get-value
             set-value!)
    
    ; Fields -------------------------------------
    
    ; (cell (alistof (U boolean symbol number) string))
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
      (cond [(not raw)              #f]
            [(equal? raw "--yes--") #t]
            [(equal? raw "--no--")  #f]
            [(string->number raw) => (lambda (num) num)]
            [else (string->symbol raw)]))
    
    ; (U boolean symbol number) ->  string
    (define/override (option->string option)
      (or (assoc-value/default option (web-cell-ref options-cell) #f)
          (error (format "Not a valid option: ~s ~s" (web-cell-ref options-cell) option))))))

; Helpers ----------------------------------------

; (U string symbol) -> string
(define (string+symbol->string val)
  (if (string? val)
      val
      (symbol->string val)))

; Provide statements -----------------------------

(provide vanilla-combo-box%
         combo-box%)
