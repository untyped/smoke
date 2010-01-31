#lang scheme

(require "../../lib-base.ss")

(require (planet untyped/unlib:3/enumeration)
         (planet untyped/unlib:3/string))

; Struct types -----------------------------------

; (struct string (string -> any) (any -> string))
(define-struct arg (pattern decoder encoder) #:transparent)

; (struct (-> regexp) (listof arg) (listof (U string arg)))
(define-struct pattern (regexp-maker args elements) #:transparent)

; (struct pattern (box page<%>))
(define-struct rule (pattern page-box) #:transparent)

; (struct string continuation-marks)
(define-struct (exn:fail:dispatch exn) () #:transparent)

; Constructors -----------------------------------

; (U string arg) ... -> pattern
(define (create-pattern . elements)
  (make-pattern (make-regexp-maker elements)
                (filter arg? elements)
                elements))

; Configuration ----------------------------------

(define-enum link-substitutes (hide span body))

; (parameter link-format)
(define default-link-substitute
  (make-parameter (link-substitutes body)))

; Helpers ----------------------------------------

; (listof (U string (-> string) arg)) ... -> string
(define (make-regexp-maker elements)
  (let ([parts `("^"
                 ,@(for/list ([elem (in-list elements)])
                     (match elem
                       [(? string?)    (regexp-quote elem)]
                       [(? arg?)       (string-append "(" (arg-pattern elem) ")")]
                       [(? procedure?) (lambda () (regexp-quote (elem)))]))
                 "\\/?$")]) ; optional trailing slash
    (lambda ()
      (pregexp (apply string-append
                      (for/list ([part (in-list parts)])
                        (if (procedure? part)
                            (part)
                            part)))))))

; Provide statements -----------------------------

(provide link-substitutes
         (struct-out exn:fail:dispatch))

(provide/contract
 [struct arg                             ([pattern            string?]
                                          [decoder            procedure?]
                                          [encoder            procedure?])]
 [struct pattern                         ([regexp-maker       (-> regexp?)]
                                          [args               (listof arg?)]
                                          [elements           (listof (or/c string? procedure? arg?))])]
 [struct rule                            ([pattern            pattern?]
                                          [page-box           (box/c (is-a?/c page<%>))])]
 [create-pattern                         (->* () () #:rest (listof (or/c string? arg? procedure?)) pattern?)]
 [default-link-substitute                (parameter/c (enum-value/c link-substitutes))])
