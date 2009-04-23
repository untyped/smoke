#lang scheme/base

(require net/sendurl
         net/ssl-tcp-unit
         net/tcp-sig
         net/tcp-unit
         net/url
         scheme/contract
         scheme/unit
         web-server/web-server
         (except-in web-server/http redirect-to)
         web-server/configuration/namespace
         web-server/dispatchers/dispatch
         (prefix-in lift:      web-server/dispatchers/dispatch-lift)
         (prefix-in fsmap:     web-server/dispatchers/filesystem-map)
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)
         (prefix-in servlet:   web-server/dispatchers/dispatch-servlets)
         (prefix-in files:     web-server/dispatchers/dispatch-files)
         web-server/private/mime-types
         web-server/servlet/setup
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
         #:manager           [manager          (make-default-smoke-manager)]
         #:port              [port             8765]
         #:listen-ip         [listen-ip        #f]
         #:htdocs-paths      [app-htdocs-paths null]
         #:mime-types-path   [mime-types-path  smoke-mime-types-path]
         #:launch-browser?   [launch-browser?  #f]
         #:404-handler       [404-handler      smoke-404-handler]
         #:servlet-namespace [servlet-namespace-specs   null])
  (serve/dispatcher
   (make-smoke-dispatcher (make-smoke-controller servlet-start)
                          #:manager           manager
                          #:htdocs-paths      `(,@app-htdocs-paths ,smoke-htdocs-path)
                          #:mime-types-path   mime-types-path
                          #:404-handler       404-handler
                          #:servlet-namespace servlet-namespace-specs)
   #:port               port
   #:listen-ip          listen-ip
   #:launch-browser-url (and launch-browser? "/")))

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
         #:404-handler     [404-handler      smoke-404-handler])
  (serve/dispatcher
   (make-smoke-dispatcher (if run-tests?
                              (make-delirium-controller (make-smoke-controller start) test run-tests)
                              (make-smoke-controller start))
                          #:manager         manager
                          #:htdocs-paths    `(,delirium-htdocs-path ,@app-htdocs-paths ,smoke-htdocs-path)
                          #:mime-types-path mime-types-path                       
                          #:404-handler     404-handler)
   #:port               port
   #:listen-ip          listen-ip
   #:launch-browser-url (and launch-browser? "/test")))

; Helpers ----------------------------------------

; (-> response) -> (request -> response)
(define (make-smoke-controller start)
  (lambda (request)
    (start)))

;  (connection request -> void)
;   #:port               natural
;   #:listen-ip          (U string #f)
;  [#:ssl?               boolean]
;  [#:server-cert-path   (U path #f)]
;  [#:private-key-path   (U path #f)]
;   #:launch-browser-url (U string #f)
; ->
;  (connection request -> void)
(define (serve/dispatcher
         dispatcher
         #:port               port
         #:listen-ip          listen-ip
         #:ssl?               [ssl?              #f]
         #:server-cert-path   [servlet-cert-path (build-path (current-directory) "server-cert.pem")]
         #:private-key-path   [private-key-path  (build-path (current-directory) "private-key.pem")]
         #:launch-browser-url launch-browser-url)
  
  (define standalone-url
    (format "~a://localhost~a/"
            (if ssl? "https" "http")
            (if ssl?
                (if (= port 443) "" (format ":~a" port))
                (if (= port 80)  "" (format ":~a" port)))))
  
  (define tcp-unit
    (if ssl?
        (let ()
          (define-unit-binding ssl-tcp@
            (make-ssl-tcp@ servlet-cert-path private-key-path #f #f #f #f #f)
            (import)
            (export tcp^))
          ssl-tcp@)
        tcp@))
  
  (define shutdown-server
    (serve #:dispatch  dispatcher
           #:listen-ip listen-ip
           #:port      port
           #:tcp@      tcp-unit))
  
  (define sema
    (make-semaphore 0))
  
  (printf "Application running at ~a.\n" standalone-url)
  
  (when launch-browser-url
    (send-url launch-browser-url #t))
  
  (let ([sema (make-semaphore)]
        [bye  (lambda ()
                (printf "\nWeb Server stopped.\n")
                (shutdown-server))])
    (with-handlers ([exn:break? (lambda (exn) (bye))])
      (semaphore-wait/enable-break sema)
      ; We can add code to increment sema, which will bring us here.
      (bye))))

(define (make-smoke-dispatcher
         start
         #:manager                   [manager                   (make-default-smoke-manager)]
         #:servlet-namespace         [servlet-namespace-specs   null]
         #:servlet-current-directory [servlet-current-directory (current-directory)]
         #:htdocs-paths              [htdocs-paths              (list smoke-htdocs-path)]
         #:mime-types-path           [mime-types-path           smoke-mime-types-path]
         #:404-handler               [404-handler               smoke-404-handler])
  
  ; #:additional-specs (listof require-spec) -> namespace
  (define make-servlet-namespace
    (make-make-servlet-namespace #:to-be-copied-module-specs servlet-namespace-specs))
  
  ; (box (U servlet #f))
  (define servlet-box
    (box #f))
  
  ; url -> servlet
  (define (url->servlet url)
    (or (unbox servlet-box)
        (let ([servlet (parameterize
                           ([current-custodian (make-custodian)]
                            [current-namespace (make-servlet-namespace #:additional-specs default-module-specs)])
                         (make-v2.servlet servlet-current-directory manager start))])
          (set-box! servlet-box servlet)
          servlet)))
  
  ; connection request -> void
  (apply sequencer:make
         `(,(lambda (conn req)
              (current-request-set! req)
              (next-dispatcher))
           ,(servlet:make url->servlet)
           ,@(map (lambda (path)
                    (files:make #:url->path       (fsmap:make-url->path path)
                                #:path->mime-type (make-path->mime-type mime-types-path)
                                #:indices         (list "index.html" "index.htm")))
                  htdocs-paths)
           ,(lift:make 404-handler))))

; request -> response
(define (smoke-404-handler request)
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
                                        #:port              natural-number/c
                                        #:listen-ip         (or/c string? false/c)
                                        #:htdocs-paths      (listof path?)
                                        #:mime-types-path   path?
                                        #:launch-browser?   boolean?
                                        #:404-handler       (-> request? response?)
                                        #:servlet-namespace list?)
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
                             void?)])
