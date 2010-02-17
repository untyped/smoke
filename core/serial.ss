#lang scheme

(require "../base.ss")

(require file/md5
         web-server/http
         web-server/http/bindings
         "request.ss"
         "session.ss")

; Public -----------------------------------------

; -> string
(define (generate-callback-serial [seed "Smoke"])
  (let ([time (current-inexact-milliseconds)])
    (md5/string (string-append seed (number->string time)))))

; request [string] -> string
(define (generate-web-frame-serial
         request
         [callback-serial (or (request-callback-serial request)
                              (error "request has no callback serial" request))])
  (let ([user-agent (or (request-header-ref request 'User-Agent)
                        (request-header-ref request 'user-agent)
                        (request-header-ref request 'User-agent)
                        (error "request has no user-agent" request))]
        [session-id (request-session-id request)])
    (md5/string (string-append callback-serial user-agent session-id))))

; request -> (U string #f)
(define (request-callback-serial request)
  (request-binding-ref request '__k))

; Helpers ----------------------------------------

; string -> string
(define (md5/string str)
  (bytes->string/utf-8 (md5 (string->bytes/utf-8 str))))

; Provides ---------------------------------------

(provide/contract
 [generate-callback-serial  (->* () (string?) string?)]
 [generate-web-frame-serial (->* (request?) (string?) string?)]
 [request-callback-serial   (-> request? (or/c string? #f))])
