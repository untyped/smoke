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
         "env.ss")

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

; (url-string -> response) -> request
(define (send/suspend response-generator)
  (let ([site (current-site)]
        [page (current-page)])
    (call-with-composable-continuation
     (lambda (k)
       (let* ([instance-id (current-servlet-instance-id)]
              [ctxt        (current-execution-context)]
              [k-embedding ((manager-continuation-store! (current-servlet-manager))
                            instance-id
                            (make-custodian-box (current-custodian) k)
                            (current-servlet-continuation-expiration-handler))]
              [k-url        (embed-ids 
                             (list* instance-id k-embedding)
                             (request-uri (execution-context-request ctxt)))]
              [wrapper      (lambda (k-url)
                              (current-site-set! site)
                              (current-page-set! page)
                              (response-generator k-url))])
         (send/back (wrapper k-url))))
     servlet-prompt)))

; (url -> response) -> request
(define (send/suspend/url response-generator)
  (send/suspend
   (lambda (k-url)
     (response-generator (string->url k-url)))))

; (url-string -> response) -> request
(define (send/forward response-generator)
  (clear-continuation-table!)
  (send/suspend response-generator))

; (((-> response) -> url-string) -> response) -> request
(define (send/suspend/dispatch response-generator)
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
                              (let ([new-request (send/suspend k1)])
                                (k0 wrapper)))
                            servlet-prompt)))))))
                 servlet-prompt)])
    (thunk)))

; (((-> response) -> url) -> response) -> request
(define (send/suspend/url/dispatch response-generator)
  (send/suspend/dispatch
   (lambda (embed/url)
     (response-generator
      (lambda (proc)
        (string->url (embed/url proc)))))))

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
         clear-continuation-table!)

(provide/contract
 [send/back                 (-> response/c void?)]
 [send/finish               (-> response/c void?)]
 [send/forward              (-> response-generator/c request?)]
 [send/suspend              (-> response-generator/c request?)]
 [send/suspend/dispatch     (-> (-> smoke-embed/url/c response/c) any/c)]
 [send/suspend/url          (-> (-> url? response/c) request?)]
 [send/suspend/url/dispatch (-> (-> (-> (-> any) url?) response/c) any/c)]
 [redirect/get              (-> request?)]
 [redirect/get/forget       (-> request?)]
 [with-errors-to-browser    (-> (-> response/c request?) (-> any) any/c)])
