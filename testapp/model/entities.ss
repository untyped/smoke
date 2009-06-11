#lang scheme/base

(require "db.ss")

(define-entity post
  ([subject string #:allow-null? #f #:max-length 128]
   [content string])
  #:pretty-formatter
  (lambda (post)
    (post-subject post)))

(define-entity kitchen-sink
  ([a-boolean                 boolean]
   [a-integer                 integer]
   [a-real                    real]
   [a-string                  string]
   [a-symbol                  symbol]
   [a-10-char-string          string    #:max-length 10]
   [a-10-char-symbol          symbol    #:max-length 10]
   [a-time-utc                time-utc]
   [a-time-tai                time-tai]
   [a-post                    post]
   [a-required-integer        integer   #:allow-null? #f]
   [a-required-real           real      #:allow-null? #f]
   [a-required-string         string    #:allow-null? #f]
   [a-required-symbol         symbol    #:allow-null? #f]
   [a-required-10-char-string string    #:allow-null? #f #:max-length 10]
   [a-required-10-char-symbol symbol    #:allow-null? #f #:max-length 10]
   [a-required-time-utc       time-utc  #:allow-null? #f]
   [a-required-time-tai       time-tai  #:allow-null? #f]
   [a-required-post           post      #:allow-null? #f]))

(provide-entity/contract post)
(provide-entity/contract kitchen-sink)
