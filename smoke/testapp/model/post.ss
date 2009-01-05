#lang scheme/base

(require "db.ss")

(define-persistent-struct post
  ([subject type:string]
   [content type:string])
  #:table-name 'Posts)

; Provide statements -----------------------------

(provide (persistent-struct-out post))
