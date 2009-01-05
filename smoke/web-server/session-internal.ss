#lang scheme/base

(require scheme/contract
         (planet untyped/unlib:3/time)
         "cookie-util.ss")

; Variables --------------------------------------

; (hashof string session)
(define sessions (make-hash))

; Structure types --------------------------------

; (struct string time-utc time-utc (hasheqof symbol any))
(define-struct session
  (cookie-id issued [accessed #:mutable] hash)
  #:transparent)

; Accessors and mutators -------------------------

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
 [sessions            hash?]
 [struct session      ([cookie-id string?]
                       [issued    time-utc?]
                       [accessed  time-utc?]
                       [hash      (and/c hash? hash-eq?)])]
 [session-cookie-name string?]
 [session-set?        (-> session? symbol? boolean?)]
 [session-ref         (-> session? symbol? any)]
 [session-set!        (-> session? symbol? any/c void?)]
 [session-remove!     (-> session? symbol? void?)])
