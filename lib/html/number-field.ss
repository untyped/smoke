#lang scheme/base

(require srfi/13
         "../../lib-base.ss"
         "text-field.ss")

(define number-field%
  (class/cells text-field% ()
    
    (inherit get-allow-blank?
             set-raw!)
    
    ; Fields -------------------------------------
    
    ; (cell (U number #f))
    (init-field min-value #f #:accessor #:mutator)
    
    ; (cell (U number #f))
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
            [(not (number? num))   (raise-value-exn this blank? min max)]
            [(and min (< num min)) (raise-value-exn this blank? min max)]
            [(and max (> num max)) (raise-value-exn this blank? min max)]
            [else                  num]))
    
    ; (U string #f) -> void
    (define/override (set-value! val)
      (set-raw! (cond [(not val)      ""]
                      [(number? val) (number->string val)]
                      [else           (raise-type-error 'set-value! "(U number #f)" val)])))))

; Helpers ----------------------------------------

; form-element<%> boolean (U number #f) (U number #f) -> void
(define (raise-value-exn element blank? min max)
  ; We currently ignore the blank? argument because it causes misleading error messages with Scaffold.
  ; For backwards-compatibility, Scaffold does not use allow-blank?=#f, favouring errors in the validation phase instead of the parse phase.
  ; This means Smoke can create misleading error messages when a field is required in Scaffold but not in Smoke.
  (cond [(and min max) (raise-exn exn:smoke:form (format "This value must be a number between ~a and ~a inclusive." min max) element)]
        [min           (raise-exn exn:smoke:form (format "This value must be a number greater than or equal to ~a." min) element)]
        [max           (raise-exn exn:smoke:form (format "This value must be a number less than or equal to ~a." max) element)]
        [else          (raise-exn exn:smoke:form (format "This value must be a number.") element)]))

; Provide statements -----------------------------

(provide number-field%)