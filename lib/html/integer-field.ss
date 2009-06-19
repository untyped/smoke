#lang scheme/base

(require srfi/13
         "../../lib-base.ss"
         "text-field.ss")

(define integer-field%
  (class/cells text-field% ()
    
    (inherit get-allow-blank?
             set-raw!)
    
    ; Fields -------------------------------------
    
    ; (cell (U integer #f))
    (init-field min-value #f #:accessor #:mutator)
    
    ; (cell (U integer #f))
    (init-field max-value #f #:accessor #:mutator)
    
    ; Public methods -----------------------------
    
    ; -> (U string #f)
    (define/override (get-value)
      (define str    (super get-value))
      (define num    (and str (string->number str)))
      (define blank? (get-allow-blank?))
      (define min    (get-min-value))
      (define max    (get-max-value))
      (cond [(not str)             #f]
            [(not num)             (raise-value-exn this blank? min max)]
            [(not (integer? num))  (raise-value-exn this blank? min max)]
            [(and min (< num min)) (raise-value-exn this blank? min max)]
            [(and max (> num max)) (raise-value-exn this blank? min max)]
            [else                  num]))
    
    ; (U string #f) -> void
    (define/override (set-value! val)
      (set-raw! (cond [(not val)      ""]
                      [(integer? val) (number->string val)]
                      [else           (raise-type-error 'set-value! "(U integer #f)" val)])))))

; Helpers ----------------------------------------

; form-element<%> boolean (U integer #f) (U integer #f) -> void
(define (raise-value-exn element blank? min max)
  (define type-string
    (if blank? 
        "blank or a whole number"
        "a whole number"))
  (cond [(and min max) (raise-exn exn:smoke:form (format "This value must be ~a between ~a and ~a inclusive." type-string min max) element)]
        [min           (raise-exn exn:smoke:form (format "This value must be ~a greater than or equal to ~a." type-string min) element)]
        [max           (raise-exn exn:smoke:form (format "This value must be ~a less than or equal to ~a." type-string max) element)]
        [else          (raise-exn exn:smoke:form (format "This value must be ~a." type-string) element)]))

; Provide statements -----------------------------

(provide integer-field%)
