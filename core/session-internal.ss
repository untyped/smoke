#lang scheme

(require file/md5
         srfi/19
         srfi/26
         (planet untyped/unlib:3/time)
         "cookie.ss")

; Cookie name ------------------------------------

; [string] -> string
(define (generate-cookie-name [prefix "smoke"])
  ((compose (cut format "~a~a" prefix <>)
            string-downcase
            (cut substring <> 0 4)
            bytes->string/utf-8
            md5
            string->bytes/utf-8
            number->string)
   (current-inexact-milliseconds)))

; string
(define session-cookie-name (make-parameter (generate-cookie-name)))

; Session cache ----------------------------------

; (hashof string session)
(define sessions (make-hash))

; Session structs --------------------------------

; (struct string time-utc time-utc (U time-utc #f) (hasheqof symbol any))
(define-struct session
  (cookie-id issued [accessed #:mutable] [expires #:mutable] hash)
  #:transparent)

; -> string
(define (generate-session-id)
  (string->immutable-string
   (bytes->string/utf-8
    (md5 (string->bytes/utf-8 (number->string (random)))))))

; (U time-utc #f) -> void
(define (create-session expires)
  (let* ([id  (generate-session-id)]
         [now (current-time time-utc)]
         [ans (make-session id now now expires (make-hasheq))])
    (hash-set! sessions id ans)
    ans))

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
 [create-session           (-> (or/c time-utc? #f) session?)]
 [session-set?             (-> session? symbol? boolean?)]
 [session-ref              (-> session? symbol? any)]
 [session-set!             (-> session? symbol? any/c void?)]
 [session-remove!          (-> session? symbol? void?)])
