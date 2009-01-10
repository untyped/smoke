#lang scheme/base

(require "../../lib-base.ss"
         "text-field.ss")

(define regexp-field%
  (class/cells text-field% ()
    
    (inherit get-allow-blank?)
    
    ; Fields -------------------------------------
    
    ; (cell (U regexp string))
    (init-cell regexp
      #:accessor #:mutator)
    
    ; (cell (U string #f))
    (init-cell [format-string #f]
      #:accessor #:mutator)
    
    ; Public methods -----------------------------
    
    ; -> (U string #f)
    (define/override (get-value)
      (define val (super get-value))
      (define rx  (get-regexp))
      (cond [(not val) #f]
            [(regexp-match rx val) val]
            [else (let ([fmt (or (get-format-string)
                                 (and (string? rx) rx)
                                 (error "format-string is #f and regexp is not a printable value"))])
                    (raise-exn exn:smoke:form
                      (format "This value must be in the format ~s" fmt) this))]))
    
    ; -> (U (listof string) #f)
    (define (get-match)
      (define val (super get-value))
      (and val (regexp-match (get-regexp) val)))))

; Provide statements -----------------------------

(provide regexp-field%)
