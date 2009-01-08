#lang scheme/base

(require net/url
         scheme/contract
         web-server/http/request-structs
         web-server/http/response-structs
         web-server/servlet-env
         (planet untyped/delirium:3)
         (planet untyped/mirrors:1)
         "../base.ss"
         "current-request.ss"
         "send-suspend-dispatch.ss"
         "session.ss"
         "smoke-lru.ss")

;  (-> response)
;  [#:manager         manager]
;  [#:htdocs-path     (listof path)]
;  [#:port            integer]
;  [#:listen-ip       (U string #f)]
;  [#:htdocs-path     (listof path)]
;  [#:mime-types-path path]
; ->
;  void
(define (serve/smoke
         servlet-start
         #:manager         [manager          (make-default-smoke-manager)]
         #:port            [port             8765]
         #:listen-ip       [listen-ip        #f]
         #:htdocs-paths    [app-htdocs-paths null]
         #:mime-types-path [mime-types-path  smoke-mime-types-path]
         #:launch-browser? [launch-browser?  #f]
         #:404-handler     [404-handler      default-404-handler])
  (serve/servlet
   (make-smoke-controller servlet-start)
   #:command-line?            #t
   #:manager                  manager
   #:port                     port
   #:listen-ip                listen-ip
   #:servlet-path             "/"
   #:servlet-regexp           #rx"^."
   #:extra-files-paths        `(,@app-htdocs-paths ,smoke-htdocs-path)
   #:mime-types-path          mime-types-path
   #:launch-browser?          launch-browser?
   #:file-not-found-responder 404-handler))

;  (-> response) 
;  schemeunit-test
;  [#:run-tests?      boolean]
;  [#:run-tests       (-> schemeunit-test any)]
;  [#:manager         manager]
;  [#:port            integer]
;  [#:listen-ip       (U string #f)]
;  [#:htdocs-paths    (listof path)]
;  [#:mime-types-path path]
;  [#:launch-browser? boolean]
;  [#:404-handler     (request -> response)]
; ->
;  void
(define (serve/smoke/delirium
         start
         test
         #:run-tests?      [run-tests?       #t]
         #:run-tests       [run-tests        test/text-ui/pause-on-fail]
         #:manager         [manager          (make-default-smoke-manager)]
         #:port            [port             8765]
         #:listen-ip       [listen-ip        #f]
         #:htdocs-paths    [app-htdocs-paths null]
         #:mime-types-path [mime-types-path  smoke-mime-types-path]
         #:launch-browser? [launch-browser?  #f]
         #:404-handler     [404-handler      default-404-handler])
  (serve/delirium
   (make-smoke-controller start)
   test
   #:run-tests?               run-tests?
   #:run-tests                run-tests
   #:manager                  manager
   #:port                     port
   #:listen-ip                listen-ip
   #:extra-files-paths        `(,@app-htdocs-paths ,smoke-htdocs-path)
   #:mime-types-path          mime-types-path
   #:launch-browser?          launch-browser?
   #:file-not-found-responder 404-handler))

; (-> response) -> (request -> response)
(define (make-smoke-controller start)
  (lambda (request)
    (current-request-set! request)
    (start)))

; Helpers ----------------------------------------

; request -> response
(define (default-404-handler request)
  (debug "404 not found" (url->string (request-uri request)))
  (make-html-response
   #:code    404
   #:message "Not found"
   (xml (html (head (title "404 not found"))
              (body (p "Sorry! We could not find that file or resource on our server:")
                    (blockquote (tt ,(url->string (request-uri request)))))))))

; Provide statements -----------------------------

(provide/contract
 [serve/smoke           (->* ((-> response?))
                             (#:manager any/c
                                        #:port            natural-number/c
                                        #:listen-ip       (or/c string? false/c)
                                        #:htdocs-paths    (listof path?)
                                        #:mime-types-path path?
                                        #:launch-browser? boolean?
                                        #:404-handler     (-> request? response?))
                             void?)]
 [serve/smoke/delirium  (->* ((-> response?) any/c)
                             (#:manager any/c
                                        #:run-tests?      boolean?
                                        #:run-tests       (-> any/c any)
                                        #:port            natural-number/c
                                        #:listen-ip       (or/c string? false/c)
                                        #:htdocs-paths    (listof path?)
                                        #:mime-types-path path?
                                        #:launch-browser? boolean?
                                        #:404-handler     (-> request? response?))
                             void?)]
 [make-smoke-controller (-> (-> response?) (-> request? response?))])
