#lang web-server

(require file/md5
         scheme/contract
         srfi/19
         (planet untyped/unlib:3/time)
         "cookie.ss"
         "servlet.ss")

; Helpers ----------------------------------------

; [string] -> string
(define (generate-cookie-name [prefix "Smoke"])
  (format "~a~a"
          prefix
          (substring (bytes->string/utf-8 
                      (md5 (string->bytes/utf-8
                            (number->string (current-inexact-milliseconds)))))
                     0 4)))

; Cookie name ------------------------------------

; string
(define session-cookie-name
  (make-parameter (generate-cookie-name "SmokeSession")))

; Session cache ----------------------------------

; (hashof string session)
(define sessions (make-hash))

; Session structs --------------------------------

; (struct string time-utc time-utc (U time-utc #f) (hasheqof symbol any))
(define-struct session (cookie-id issued [accessed #:mutable] [expires #:mutable] hash) #:transparent)

; session symbol -> boolean
(define (session-set? session key)
  (with-handlers ([exn? (lambda _ #f)])
    (hash-ref (session-hash session) key)
    #t))

; session symbol -> any
(define (session-ref session key)
  (hash-ref (session-hash session) key #f))

; session symbol any -> nothing
(define (session-set! session key val)
  (hash-set! (session-hash session) key val))

; session symbol -> nothing
(define (session-remove! session key)
  (hash-remove! (session-hash session) key))

; Provide statements -----------------------------

(provide/contract
 [session-cookie-name      (parameter/c string?)]
 [sessions                 hash?]
 [struct session           ([cookie-id string?]
                            [issued    time-utc?]
                            [accessed  time-utc?]
                            [expires   (or/c time-utc? #f)]
                            [hash      (and/c hash? hash-eq?)])]
 [session-set?             (-> session? symbol? boolean?)]
 [session-ref              (-> session? symbol? any)]
 [session-set!             (-> session? symbol? any/c void?)]
 [session-remove!          (-> session? symbol? void?)])
