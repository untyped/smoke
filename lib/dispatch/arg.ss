#lang scheme

(require "../../lib-base.ss")

(require net/uri-codec
         srfi/19
         (planet untyped/unlib:3/enumeration)
         (planet untyped/unlib:3/time)
         "core.ss")

; -> arg
(define (boolean-arg)
  (make-arg 
   "yes|no|true|false|y|n|t|f|1|0" 
   (lambda (raw)
     (and (member (string-downcase raw) 
                  (list "yes" "true" "y" "t" "1"))
          #t))
   (lambda (arg)
     (if arg "yes" "no"))))

; -> arg
(define (integer-arg)
  (make-arg 
   "[-]?[0-9]+" 
   (lambda (raw)
     (string->number raw))
   (lambda (arg)
     (if (integer? arg)
         (number->string arg)
         (raise-type-error 'integer-arg "integer" arg)))))

; -> arg
(define (number-arg)
  (make-arg 
   "[-]?[0-9]+|[-]?[0-9]*.[0-9]*?" 
   (lambda (raw)
     (string->number raw))
   (lambda (arg)
     (cond [(integer? arg) (number->string arg)]
           [(real? arg)    (number->string (exact->inexact arg))]
           [else           (raise-type-error 'real-arg "real" arg)]))))

; -> arg
(define (string-arg)
  (make-arg 
   "[^/]+"
   (lambda (raw)
     (uri-decode raw))
   (lambda (arg)
     (if (string? arg)
         (uri-encode arg)
         (raise-type-error 'string-arg "string" arg)))))

; -> arg
(define (symbol-arg)
  (make-arg 
   "[^/]+"
   (lambda (raw)
     (string->symbol (uri-decode raw)))
   (lambda (arg)
     (if (symbol? arg)
         (uri-encode (symbol->string arg))
         (raise-type-error 'symbol-arg "symbol" arg)))))

; -> arg
(define (url-arg)
  (make-arg 
   "[^/]+"
   (lambda (raw)
     (string->url (uri-decode raw)))
   (lambda (arg)
     (cond [(string? arg) (uri-encode arg)]
           [(url? arg)    (uri-encode (url->string arg))]
           [else          (raise-type-error 'string-arg "string" arg)]))))

(define (time-utc-arg [fmt (current-time-format)])
  (make-arg
   "[^/]+"
   (lambda (raw)
     (let ([date (safe-string->date raw fmt)])
       (if date 
           (date->time-utc date)
           (raise-exn exn:fail:dispatch "no match for time-utc-arg"))))
   (lambda (time)
     (if (time-utc? time)
         (date->string (time-utc->date time) fmt)
         (raise-type-error 'time-utc-arg "time-utc" time)))))

; [natural] -> arg
(define (rest-arg [min-length 0])
  (make-arg
   (if (zero? min-length)
       ".*"
       (format ".{~a,}.*" min-length))
   (lambda (raw)
     (uri-decode raw))
   (lambda (arg)
     (if (string? arg)
         (uri-encode arg)
         (raise-type-error 'rest-arg "string" arg)))))

; enum -> arg
(define (enum-arg enum)
  (make-arg
   (string-join (map regexp-quote
                     (map (cut format "~a" <>)
                          (enum-values enum)))
                "|")
   (lambda (raw)
     (for/or ([val (in-list (enum-values enum))])
       (and (equal? (format "~a" val) raw) val)))
   (lambda (val)
     (if (enum-value? enum val)
         (format "~a" val)
         (raise-type-error (enum-name enum) (format "~a" (enum-values enum)) val)))))

; Helpers ----------------------------------------

(define (safe-string->date str fmt)
  (with-handlers ([exn? (lambda _ #f)])
    (string->date str fmt)))

; Provide statements -----------------------------

(provide/contract
 [boolean-arg  (-> arg?)]
 [integer-arg  (-> arg?)]
 [number-arg   (-> arg?)]
 [string-arg   (-> arg?)]
 [symbol-arg   (-> arg?)]
 [url-arg      (-> arg?)]
 [time-utc-arg (->* () (string?) arg?)]
 [rest-arg     (->* () (natural-number/c) arg?)]
 [enum-arg     (-> enum? arg?)])
