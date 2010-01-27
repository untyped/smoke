#lang web-server

(require srfi/13
         (planet untyped/unlib:3/string)
         "../../lib-base.ss"
         "form-element.ss")

(define text-input<%>
  (interface (form-element<%>)
    get-raw
    set-raw!))

(define text-input%
  (class/cells form-element% (text-input<%>)
    
    (inherit get-id
             get-enabled?)
    
    ; Fields -------------------------------------
    
    ; (cell string)
    (cell raw "" #:accessor)
    
    ; (cell boolean)
    (init-cell allow-blank? #t #:accessor #:mutator)
    
    ; (cell (U 'uppercase 'lowercase #f))
    (cell case-conversion #f #:accessor)
    
    ; Constructor --------------------------------
    
    ; (U 'uppercase 'lowercase #f)
    (init [case-conversion #f])
    (set-case-conversion! case-conversion)
    
    ; (U string #f)
    (init [value #f])
    (set-value! value)
    
    ; (listof symbol)
    (init [classes null])
    (super-new [classes (cons 'smoke-text-input classes)])
    
    ; Public methods -----------------------------
    
    ; (U 'uppercase 'lowercase #f) -> void
    (define/public (set-case-conversion! val)
      (if (memq val '(uppercase lowercase #f))
          (web-cell-set! case-conversion-cell val)
          (error "expected (U 'uppercase 'lowercase #f)" val)))
    
    ; string -> void
    (define/public (set-raw! val)
      (web-cell-set!
       raw-cell
       (case (get-case-conversion)
         [(uppercase) (string-upcase val)]
         [(lowercase) (string-downcase val)]
         [else    val])))
    
    ; -> (U string #f)
    (define/override (get-value)
      (define trimmed (string-trim-both (get-raw)))
      (if (equal? trimmed "")
          (if (get-allow-blank?)
              #f
              (raise-exn exn:smoke:form "This field is required: you must fill it in." this))
          trimmed))
    
    ; (U string #f) -> void
    (define/override (set-value! val)
      (set-raw! (cond [(not val)     ""]
                      [(string? val) val]
                      [else (raise-type-error 'set-value! "(U string #f)" val)])))
        
    ; -> boolean
    (define/override (value-changed?)
      (web-cell-changed? raw-cell))
    
    ; seed -> js
    (define/augment (get-on-focus seed)
      (js (= (!dot Smoke focusedId) ,(get-id))
          ,(inner (js) get-on-focus seed)))
    
    ; seed -> js
    (define/augment (get-on-blur seed)
      (js (= (!dot Smoke focusedId) null)
          ,(inner (js) get-on-blur seed)))
    
    ; seed -> js
    (define/augride (get-on-change seed)
      (define id (get-id))
      (js (!dot Smoke (setSubmitData ,id (!dot Smoke (findById ,id) value)))))))

; Provide statements -----------------------------

(provide text-input<%> text-input%)
