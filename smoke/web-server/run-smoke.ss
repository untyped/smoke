#lang scheme/base

(require net/url
         scheme/contract
         web-server/http/request-structs
         web-server/http/response-structs
         web-server/servlet-env
         (planet untyped/delirium:2)
         (planet untyped/mirrors:1)
         "../base.ss"
         "current-request.ss"
         "send-suspend-dispatch.ss"
         "session.ss"
         "smoke-lru.ss")

;  (-> response) 
;  [#:htdocs-path     (listof path)]
;  [#:port            integer]
;  [#:listen-ip       (U string #f)]
;  [#:htdocs-path     (listof path)]
;  [#:mime-types-path path]
; ->
;  void
(define (run-smoke
         #:start           servlet-start
         #:manager         [manager         (make-default-smoke-manager)]
         #:port            [port            8765]
         #:listen-ip       [listen-ip       #f]
         #:htdocs-path     [app-htdocs-path null]
         #:mime-types-path [mime-types-path smoke-mime-types-path]
         #:launch-browser? [launch-browser? #f]
         #:404-handler     [404-handler default-404-handler])
  (serve/servlet
   (make-smoke-controller servlet-start)
   #:command-line?            #t
   #:manager                  manager
   #:port                     port
   #:listen-ip                listen-ip
   #:servlet-path             "/"
   #:servlet-regexp           #rx"^."
   #:extra-files-paths        (append app-htdocs-path (list smoke-htdocs-path))
   #:mime-types-path          mime-types-path
   #:launch-browser?          launch-browser?
   #:file-not-found-responder 404-handler))

;  (-> response) 
;  [#:htdocs-path     (listof path)]
;  [#:port            integer]
;  [#:listen-ip       (U string #f)]
;  [#:htdocs-path     (listof path)]
;  [#:mime-types-path path]
; ->
;  void
(define (run-smoke/delirium
         #:start           servlet-start
         #:tests           tests
         #:run-tests?      [run-tests?      #t]
         #:manager         [manager         (make-default-smoke-manager)]
         #:port            [port            8765]
         #:listen-ip       [listen-ip       #f]
         #:htdocs-path     [app-htdocs-path null]
         #:mime-types-path [mime-types-path smoke-mime-types-path]
         #:launch-browser? [launch-browser? #f]
         #:404-handler     [404-handler default-404-handler])
  (serve/servlet
   (if run-tests?
       (make-delirium-controller tests (make-smoke-controller servlet-start))
       (make-smoke-controller servlet-start))
   #:command-line?            #t
   #:launch-browser?          launch-browser?
   #:manager                  manager
   #:port                     port
   #:listen-ip                listen-ip
   #:servlet-path             "/test"
   #:servlet-regexp           #rx"^."
   #:extra-files-paths        (if run-tests?
                                  (append app-htdocs-path (list delirium-htdocs-path smoke-htdocs-path))
                                  (append app-htdocs-path (list smoke-htdocs-path)))
   #:mime-types-path          mime-types-path
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
 [run-smoke             (->* (#:start (-> response?))
                             (#:manager any/c
                                        #:port            natural-number/c
                                        #:listen-ip       (or/c string? false/c)
                                        #:htdocs-path     (listof path?)
                                        #:mime-types-path path?
                                        #:launch-browser? boolean?
                                        #:404-handler     (-> request? response?))
                             void?)]
 [run-smoke/delirium    (->* (#:start (-> response?)
                                      #:tests any/c)
                             (#:manager any/c
                                        #:run-tests? boolean?
                                        #:port            natural-number/c
                                        #:listen-ip       (or/c string? false/c)
                                        #:htdocs-path     (listof path?)
                                        #:mime-types-path path?
                                        #:launch-browser? boolean?
                                        #:404-handler     (-> request? response?))
                             void?)]
 [make-smoke-controller (-> (-> response?) (-> request? response?))])
