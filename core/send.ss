#lang scheme

(require "../base.ss")

(require net/url
         web-server/http
         web-server/managers/manager
         web-server/private/servlet
         web-server/private/url-param
         web-server/private/util
         (only-in web-server/servlet/servlet-structs response-generator/c)
         (prefix-in inner: web-server/servlet/web)
         "env.ss"
         "web-cell.ss"
         "web-cell-save.ss")

; Public -----------------------------------------

; url -> boolean
(define continuation-url?
  inner:continuation-url?)

; (parameter (request -> response))
(define current-servlet-continuation-expiration-handler
  inner:current-servlet-continuation-expiration-handler)

; seconds-natural -> void
(define adjust-timeout!
  inner:adjust-timeout!)

; -> void
(define clear-continuation-table!
  inner:clear-continuation-table!)

; response -> void
(define send/back
  inner:send/back)

; response -> void
(define send/finish
  inner:send/finish)

; (url-string -> response) [url] -> request
(define (send/suspend response-generator [base-url (request-uri (current-request))])
  (let ([frame (save-web-frame)]
        [site  (current-site)]
        [page  (current-page)])
    (begin0
      (call-with-composable-continuation
       (lambda (k)
         (let* ([instance-id (current-servlet-instance-id)]
                [k-embedding ((manager-continuation-store! (current-servlet-manager))
                              instance-id
                              (make-custodian-box (current-custodian) k)
                              (current-servlet-continuation-expiration-handler))]
                [k-url       (embed-ids (list* instance-id k-embedding) base-url)])
           (send/back (response-generator k-url))))
       servlet-prompt)
      (current-site-set! site)
      (current-page-set! page)
      (restore-web-frame frame))))

; (url -> response) [url] -> request
(define (send/suspend/url response-generator [base-url (request-uri (current-request))])
  (send/suspend
   (lambda (k-url)
     (response-generator (string->url k-url)))
   base-url))

; (url-string -> response) [url] -> request
(define (send/forward response-generator [base-url (request-uri (current-request))])
  (clear-continuation-table!)
  (send/suspend response-generator base-url))

; (((-> response) -> url-string) -> response) [url] -> request
(define (send/suspend/dispatch response-generator [base-url (request-uri (current-request))])
  (let* ([site  (current-site)]
         [page  (current-page)]
         ; This restores the tail position:
         [thunk (call-with-current-continuation
                 (lambda (k0)
                   (send/back
                    (response-generator
                     (lambda (proc)
                       (let ([wrapper (lambda ()
                                        (current-site-set! site)
                                        (current-page-set! page)
                                        (proc))])
                         (let/ec k1 
                           ; This makes the second continuation captured by send/suspend smaller:
                           (call-with-continuation-prompt
                            (lambda ()
                              (let ([new-request (send/suspend k1 base-url)])
                                (k0 wrapper)))
                            servlet-prompt)))))))
                 servlet-prompt)])
    (thunk)))

; (((-> response) -> url) -> response) -> request
(define (send/suspend/url/dispatch response-generator [base-url (request-uri (current-request))])
  (send/suspend/dispatch
   (lambda (embed/url)
     (response-generator
      (lambda (proc)
        (string->url (embed/url proc)))))
   base-url))

; ((url-string -> response) -> request) -> request
(define ((make-redirect/get send/suspend))
  (send/suspend (lambda (k-url) (redirect-to k-url temporarily))))

; -> request
(define redirect/get
  (make-redirect/get send/suspend))

; -> request
(define redirect/get/forget
  (make-redirect/get send/forward))

; (request -> response) (-> any) -> any
(define (with-errors-to-browser send/finish-or-back thunk)
  (with-handlers ([exn? (lambda (exn)
                          (send/finish-or-back
                           `(html (head (title "Servlet Error"))
                                  (body ([bgcolor "white"])
                                        (p "The following error occured: "
                                           (pre ,(exn->string exn)))))))])
    (thunk)))

; Helpers ----------------------------------------

; (list number number number) url -> string
(define (embed-ids v u)
  (url->string (insert-param u "k" (write/string v))))

; contract
(define smoke-embed/url/c
  (-> (-> any/c) string?))

; Provides ---------------------------------------

(provide continuation-url?
         current-servlet-continuation-expiration-handler
         adjust-timeout!
         clear-continuation-table!
         smoke-embed/url/c)

(provide/contract
 [send/back                 (-> response/c void?)]
 [send/finish               (-> response/c void?)]
 [send/forward              (->* (response-generator/c) (url?) request?)]
 [send/suspend              (->* (response-generator/c) (url?) request?)]
 [send/suspend/dispatch     (->* ((-> smoke-embed/url/c response/c)) (url?) any/c)]
 [send/suspend/url          (->* ((-> url? response/c)) (url?) request?)]
 [send/suspend/url/dispatch (->* ((-> (-> (-> any) url?) response/c)) (url?) any/c)]
 [redirect/get              (-> request?)]
 [redirect/get/forget       (-> request?)]
 [with-errors-to-browser    (-> (-> response/c request?) (-> any) any/c)])
