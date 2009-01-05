#lang scheme/base

(require mzlib/md5
         scheme/contract)

; If two web servers are started on different ports of the same host,
; their cookie namespaces will overlap. To avoid this we generate cookie
; names using a prefix that indicates the type of cookie and a randomly
; generated suffix.

; Helpers ----------------------------------------

; string
(define cookie-suffix
  (substring (bytes->string/utf-8 
              (md5 (string->bytes/utf-8
                    (number->string (current-inexact-milliseconds)))))
             0 4))

; string -> string
(define (make-cookie-name prefix)
  (string-append prefix cookie-suffix))

; Cookie names -----------------------------------

; string
(define session-cookie-name
  (make-cookie-name "SmokeSession"))

; string
(define expiry-cookie-name
  (make-cookie-name "SmokeExpiry"))

; Provide statements -----------------------------

(provide/contract
 [session-cookie-name string?]
 [expiry-cookie-name  string?])
