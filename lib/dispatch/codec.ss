#lang scheme

(require "../../lib-base.ss")

(require "core.ss")

; URL decoding/encoding --------------------------

; pattern string -> (U list #f)
(define (pattern-decode pattern url-string)
  (let* ([regexp  ((pattern-regexp-maker pattern))]
         [matches (regexp-match regexp url-string)]
         [decoded (and matches
                       (= (length (cdr matches)) 
                          (length (pattern-args pattern)))
                       (for/list ([arg   (in-list (pattern-args pattern))]
                                  [match (in-list (cdr matches))])
                         ((arg-decoder arg) match)))])
    decoded))

; pattern list -> (U string #f)
(define (pattern-encode pattern args)
  (and (= (length (pattern-args pattern)) (length args))
       (apply string-append
              (let loop ([elems (pattern-elements pattern)]
                         [args  args])
                (match elems
                  [(list) null]
                  [(list elem elem-rest ...)
                   (match elem
                     [(? string?)    (cons elem   (loop elem-rest args))]
                     [(? procedure?) (cons (elem) (loop elem-rest args))]
                     [(? arg?)       (cons ((arg-encoder elem) (car args))
                                           (loop elem-rest (cdr args)))])])))))

; Provides ---------------------------------------

(provide/contract
 [pattern-decode (-> pattern? string? (or/c list? #f))]
 [pattern-encode (-> pattern? list? (or/c string? #f))])