#lang scheme/base

(require "db.ss")

(define-entity post
  ([subject string #:allow-null? #f]
   [content string]))

(provide-entity/contract post)
