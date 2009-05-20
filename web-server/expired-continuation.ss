#lang scheme/base

(require net/url
         scheme/contract
         (only-in web-server/servlet
                  header?
                  make-header
                  request?
                  request-uri
                  request-headers)
         (planet untyped/mirrors:1/mirrors)
         (planet untyped/unlib:3/list)
         (planet untyped/unlib:3/string)
         "cookie.ss"
         "cookie-util.ss")

; Helpers ----------------------------------------

; Accessors --------------------------------------

; request -> (U string #f)
(define (expiry-cookie-ref request)
  (define cookies (ensure-string (assoc-value/default 'cookie (request-headers request) #f)))
  (and cookies (get-cookie/single expiry-cookie-name cookies)))

; Expiry -----------------------------------------

; request -> cookie
(define (make-expiry-cookie request)
  (cookie:add-path (set-cookie expiry-cookie-name (url->string (request-uri request))) "/"))

; request -> header
(define (make-expiry-header request)
  (make-header #"Set-Cookie" (string->bytes/utf-8 (print-cookie (make-expiry-cookie request)))))

; request url -> response
(define (make-expiry-response request redirect-url)
  ; response
  (make-redirect-response
   redirect-url
   #:code    301
   #:message "Moved permanently"
   #:headers (list (make-expiry-header request)
                   (make-header #"Location" (string->bytes/utf-8 (url->string redirect-url))))))

; Cancel expiry ----------------------------------

; request -> cookie
(define (make-cancel-expiry-cookie request)
  (cookie:add-expires (cookie:add-path (set-cookie expiry-cookie-name "") "/") (- (current-seconds) (* 24 60 60))))

; request -> header
(define (make-cancel-expiry-header request)
  (make-header #"Set-Cookie" (string->bytes/utf-8 (print-cookie (make-cancel-expiry-cookie request)))))

; Provide statements -----------------------------

(provide/contract
 [expiry-cookie-ref         (-> request? (or/c string? false/c))]
 [make-expiry-cookie        (-> request? cookie?)]
 [make-expiry-header        (-> request? header?)]
 [make-expiry-response      (-> request? url? (or/c response/full? response/incremental?))]
 [make-cancel-expiry-cookie (-> request? cookie?)]
 [make-cancel-expiry-header (-> request? header?)])
