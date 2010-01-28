#lang scheme

(require (planet schematics/schemeunit:3))

(define cache (make-parameter #f))

(define (sizeof/top x)
  (parameterize ([cache (make-hasheq)])
    (sizeof/struct x)))

(define (sizeof x)
  (if (hash-has-key? (cache) x)
      (begin 0)
      (begin (hash-set! (cache) x #t)
             (cond [(string?    x)   (sizeof/string      x)]
                   [(symbol?    x)   (sizeof/symbol      x)]
                   [(bytes?     x)   (sizeof/bytes       x)]
                   [(number?    x)   (sizeof/number      x)]
                   [(char?      x)   (sizeof/char        x)]
                   [(boolean?   x)   (sizeof/boolean     x)]
                   [(null?      x)   (sizeof/null        x)]
                   [(pair?      x)   (sizeof/pair        x)]
                   [(vector?    x)   (sizeof/vector      x)]
                   [(struct?    x)   (sizeof/struct      x)]
                   [(hash?      x)   (sizeof/hash        x)]
                   [(namespace? x)   (sizeof/namespace   x)]
                   [(procedure? x)   (sizeof/procedure   x)]
                   [(struct-type? x) (sizeof/struct-type x)]
                   [else           (printf "  unknown ~a~n" x) 4]))))

;My impression is that 
; cons pairs are 3 words, 
; structs are n+2 words for n fields, 
; strings are n+1 words for n characters,
; bytes are n/4 + 1 words for a bytes-length of n, 
; vectors are n+1 words, 
; objects are n+3 words for a class with n fields, 
; classes are n+2 words for n methods, 
; some of those "+1"s could be "+2"s (or +2s being +3s, etc).

(define (sizeof/string str)
  (+ 4 ; length
     (* 4 (string-length str)))) ; characters

(define (sizeof/symbol sym)
  4)

(define (sizeof/bytes byt)
  (+ 4 (bytes-length byt)))

(define (sizeof/number num)
  4)

(define (sizeof/char char)
  4)

(define (sizeof/boolean bool)
  4)

(define (sizeof/null null)
  4)

(define (sizeof/pair pair)
  (+ 4
     (sizeof (car pair))
     (sizeof (cdr pair))))

(define (sizeof/vector vec)
  (for/fold ([accum 4])
            ([item (in-vector vec)])
            (+ accum (sizeof item))))

(define (sizeof/struct struct)
  (sizeof/vector (struct->vector struct)))

(define (sizeof/hash hash)
  (for/fold ([accum 4])
            ([(key val) (in-dict hash)])
            (+ accum (sizeof key) (sizeof val))))

(define (sizeof/namespace ns)
  (for/fold ([accum 4])
            ([name (in-list (namespace-mapped-symbols ns))])
            (+ accum (sizeof name) (sizeof (namespace-variable-value name #t #f ns)))))

(define (sizeof/procedure proc)
  4)

(define (sizeof/struct-type type)
  (for/fold ([accum 4])
            ([item (in-list
                    (call-with-values
                     (lambda ()
                       (struct-type-info type))
                     list))])
            (+ accum (sizeof item))))

(provide (rename-out [sizeof/top sizeof]))
