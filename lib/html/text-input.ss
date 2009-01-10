#lang scheme/base

(require srfi/13
         (planet untyped/unlib:3/string)
         "../../lib-base.ss"
         "form-element.ss")

(define text-input%
  (class/cells form-element% ()
    
    (inherit get-id
             get-enabled?)
    
    ; Fields -------------------------------------
    
    ; (cell string)
    (cell [raw ""] #:accessor #:mutator)
    
    ; (cell boolean)
    (init-cell [allow-blank? #t] #:accessor #:mutator)
    
    ; Constructor --------------------------------
    
    ; (U string #f)
    (init [value #f])
    
    ; (listof symbol)
    (init [classes null])
    
    (set-value! value)
    (super-new [classes (cons 'smoke-text-input classes)])
    
    ; Public methods -----------------------------
    
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
      (set-raw! (cond [(not val) ""]
                      [(string? val) val]
                      [else (raise-type-error 'set-value! "(U string #f)" val)])))
        
    ; -> boolean
    (define/override (value-changed?)
      (web-cell-changed? raw-cell))
    
    ; seed -> js
    (define/augride (get-on-change seed)
      (define id (get-id))
      (js (!dot Smoke (setSubmitData ,id (!dot Smoke (findById ,id) value)))))))

; Provide statements -----------------------------

(provide text-input%)
