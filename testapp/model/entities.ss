#lang scheme/base

(require (planet untyped/unlib:3:16/enumeration)
         "db.ss")

(define-entity post
  ([subject string #:allow-null? #f #:max-length 128]
   [content string])
  #:pretty-formatter
  (lambda (post)
    (post-subject post)))

(define-enum short-enum
  ([a _ "first option"]
   [b _ "second option"]
   [c _ "third option"]
   [d _ "fourth option"]))

(define-enum long-enum
  ([a _ "first option"]
   [b _ "second option"]
   [c _ "third option"]
   [d _ "fourth option"]
   [e _ "fifth option"]
   [f _ "getting boring"]
   [g _ "really boring"]
   [h _ "are we done yet"]))

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
   [a-short-enum              enum      #:values short-enum]
   [a-long-enum               enum      #:values long-enum]
   [a-post                    post]
   [a-required-integer        integer   #:allow-null? #f]
   [a-required-real           real      #:allow-null? #f]
   [a-required-string         string    #:allow-null? #f]
   [a-required-symbol         symbol    #:allow-null? #f]
   [a-required-10-char-string string    #:allow-null? #f #:max-length 10]
   [a-required-10-char-symbol symbol    #:allow-null? #f #:max-length 10]
   [a-required-time-utc       time-utc  #:allow-null? #f]
   [a-required-time-tai       time-tai  #:allow-null? #f]
   [a-required-short-enum     enum      #:allow-null? #f #:values short-enum]
   [a-required-long-enum      enum      #:allow-null? #f #:values long-enum]
   [a-required-post           post      #:allow-null? #f]))

(provide/contract/entities
 [entity     post]
 [entity     kitchen-sink]
 [short-enum enum?]
 [long-enum  enum?])
