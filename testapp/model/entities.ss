#lang scheme/base

(require "db.ss")

(define-entity post
  ([subject string #:allow-null? #f #:max-length 128]
   [content string]))

(define-entity test
  ([a-boolean  boolean]
   [a-integer  integer]
   [a-real     real]
   [a-string   string]
   [a-symbol   symbol]
   [a-time-utc time-utc]
   [a-time-tai time-tai]
   [a-post     post]))

(provide-entity/contract post)
(provide-entity/contract test)
