#lang scheme

(require "../../lib-base.ss")

(require (planet untyped/unlib:3/enumeration)
         "core.ss")

; Procedures -------------------------------------

; page any ... -> string
(define (controller-url page . args)
  (send (send page get-site) encode-url page args))

; page any ... -> string
(define (controller-access? page . args)
  (send/apply (send page get-site) access-allowed? args))

;  controller
;  [#:body    (U xml sexp #f)]
;  [#:id      (U string symbol #f)]
;  [#:class   (U string symbol #f)]
;  [#:classes (listof (U string symbol))]
;  [#:title   (U string #f)]
;  [#:else    (U link-substitute html)]
; ->
;  (U xml sexp (listof sexp))
(define (controller-link
         controller
         #:body    [body        #f]
         #:id      [id          #f]
         #:class   [class       #f]
         #:classes [classes     (if class (list class) null)]
         #:title   [title       #f]
         #:anchor  [anchor      #f]
         #:else    [substitute  (default-link-substitute)]
         . args)
  (let* ([access?      (apply controller-access? controller args)]
         [plain–href   (apply controller-url controller args)]
         [href         (if anchor
                           (format "~a#~a" plain–href anchor)
                           plain–href)]
         [body         (or body href)]
         [id           (and id (string+symbol->string id))]
         [class        (and (pair? classes) (string-join (map string+symbol->string classes) " "))])
    (if access?
        (xml (a (@ [href ,href]
                   ,(opt-xml-attr id)
                   ,(opt-xml-attr class)
                   ,(opt-xml-attr title)) ,body))
        (enum-case link-substitutes substitute
          [(hide) (xml)]
          [(span) (xml (span (@ ,(opt-xml-attr id)
                                ,(opt-xml-attr class class (format "no-access-link ~a" class))
                                ,(opt-xml-attr title)) ,body))]
          [(body) (xml ,body)]
          [else   substitute]))))

; Helpers ----------------------------------------

; (U symbol string) -> string
(define (string+symbol->string item)
  (if (string? item)
      item
      (symbol->string item)))

; Provide statements -----------------------------

(provide/contract
 [controller-url     (->* ((is-a?/c page<%>)) () #:rest any/c string?)]
 [controller-access? (->* ((is-a?/c page<%>)) () #:rest any/c boolean?)]
 [controller-link    (->* ((is-a?/c page<%>))
                          (#:body xml+quotable?
                                  #:id      (or/c symbol? string? #f)
                                  #:class   (or/c symbol? string? #f)
                                  #:classes (listof (or/c symbol? string?))
                                  #:title   (or/c string? #f)
                                  #:anchor  (or/c string? #f)
                                  #:else    (or/c (enum-value/c link-substitutes)
                                                  xml+quotable?))
                          #:rest any/c
                          xml?)])