#lang scheme/base

(require "../../lib-base.ss")

(require (for-syntax scheme/base
                     scheme/string
                     (planet untyped/unlib:3/debug)
                     (planet untyped/unlib:3/syntax)
                     "jquery-ui-util-internal.ss")
         "jquery-ui-util-internal.ss")

; Syntax -----------------------------------------

; (_ (U symbol string))
(define-xml-syntax !icon
  (lambda (stx)
    
    (define type?    #f)   ; boolean ; type attribute found?
    (define classes  null) ; (listof string)
    (define args-stx null) ; (listof syntax) ; reverse order
    
    ; syntax syntax -> string
    (define (class->string name-stx expr-stx)
      (let ([datum (syntax->datum expr-stx)])
        (if (or (symbol? datum) (string? datum))
            (format "~a" datum)
            (raise-syntax-error
             #f
             (format "'~a' must be an identifier or a string literal" (syntax->datum name-stx))
             stx
             #`[#,name-stx #,expr-stx]))))
    
    ; syntax -> syntax
    (define (parse-args curr-stx)
      (syntax-case* curr-stx (type class) symbolic-identifier=?
        [([type expr] rest ...)
         (let ([str (class->string #'type #'expr)])
           (cond [type? (raise-syntax-error #f "'type' specified more than once" stx #'[type expr])]
                 [(memq (string->symbol str) jquery-ui-icon-types)
                  (set! type? #t)
                  (set! classes (list* "ui-icon" (format "ui-icon-~a" str) classes))]
                 [else (raise-syntax-error #f "invalid icon type" stx #'[type expr])])
           (parse-args #'(rest ...)))]
        [([class expr] rest ...)
         (let ([str (class->string #'class #'expr)])
           (set! classes (cons str classes))
           (parse-args #'(rest ...)))]
        [([name expr] rest ...)
         (begin
           (set! args-stx (cons #'[name expr] args-stx))
           (parse-args #'(rest ...)))]
        [() (parse-finish)]))
    
    ; -> syntax
    (define (parse-finish)
      (if type?
          (with-syntax ([classes   (string-join classes " ")]
                        [(arg ...) (reverse args-stx)])
            #'(xml (span (@ [class classes] arg ...))))
          (raise-syntax-error #f "'type' argument not specified" stx)))
    
    ; syntax
    (syntax-case* stx (@) symbolic-identifier=?
      [(_ (@ [name expr] ...))
       (parse-args #'([name expr] ...))])))

; Provide statements -----------------------------

(provide (all-from-out "jquery-ui-util-internal.ss")
         !icon)
