#lang scheme

(require net/sendurl
         net/ssl-tcp-unit
         net/tcp-sig
         net/tcp-unit
         net/url
         scheme/contract
         scheme/unit
         web-server/servlet-dispatch
         web-server/web-server
         (except-in web-server/http redirect-to)
         web-server/configuration/namespace
         web-server/dispatchers/dispatch
         (prefix-in lift:      web-server/dispatchers/dispatch-lift)
         (prefix-in fsmap:     web-server/dispatchers/filesystem-map)
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)
         (prefix-in files:     web-server/dispatchers/dispatch-files)
         web-server/private/mime-types
         web-server/servlet/setup
         (planet untyped/delirium:3)
         (planet untyped/unlib:3/time)
         "../base.ss"
         "env.ss"
         "interfaces.ss"
         (prefix-in site: "dispatch-site.ss")
         (prefix-in proc: "dispatch-proc.ss"))

; Entry points -----------------------------------

;  site<%>
;  [#:htdocs-path     (listof path)]
;  [#:port            integer]
;  [#:listen-ip       (U string #f)]
;  [#:htdocs-path     (listof path)]
;  [#:mime-types-path path]
;  [#:404-handler     (-> response?)]
; ->
;  void
(define (serve/smoke
         site
         #:port              [port              8765]
         #:listen-ip         [listen-ip         #f]
         #:htdocs-paths      [site-htdocs-paths null]
         #:mime-types-path   [mime-types-path   smoke-mime-types-path]
         #:launch-browser?   [launch-browser?   #f]
         #:404-handler       [404-handler       smoke-404-handler])
  (serve/dispatcher
   (make-top-dispatcher
    site
    #:htdocs-paths      `(,@site-htdocs-paths ,smoke-htdocs-path)
    #:mime-types-path   mime-types-path
    #:404-handler       404-handler)
   #:port               port
   #:listen-ip          listen-ip
   #:launch-browser-url (and launch-browser? "/")))

;  site<%>
;  schemeunit-test
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
         site
         test
         #:run-tests       [run-tests         run-tests/pause]
         #:port            [port              8765]
         #:listen-ip       [listen-ip         #f]
         #:htdocs-paths    [site-htdocs-paths null]
         #:mime-types-path [mime-types-path   smoke-mime-types-path]
         #:launch-browser? [launch-browser?   #t]
         #:404-handler     [404-handler       smoke-404-handler])
  (serve/dispatcher
   (make-top-dispatcher
    site
    #:test-suite        test
    #:htdocs-paths      `(,@site-htdocs-paths ,smoke-htdocs-path)
    #:mime-types-path   mime-types-path
    #:404-handler       404-handler)
   #:port               port
   #:listen-ip          listen-ip
   #:launch-browser-url (and launch-browser? "/")))

; Helpers ----------------------------------------

;   (connection request -> void)
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
  
  (let* ([standalone-url  (format "~a://localhost~a/"
                                  (if ssl? "https" "http")
                                  (if ssl?
                                      (if (= port 443) "" (format ":~a" port))
                                      (if (= port 80)  "" (format ":~a" port))))]
         [tcp-unit        (if ssl?
                              (let ()
                                (define-unit-binding ssl-tcp@
                                  (make-ssl-tcp@ servlet-cert-path private-key-path #f #f #f #f #f)
                                  (import)
                                  (export tcp^))
                                ssl-tcp@)
                              tcp@)]
         [shutdown-server (serve #:dispatch  dispatcher
                                 #:listen-ip listen-ip
                                 #:port      port
                                 #:tcp@      tcp-unit)]
         [sema            (make-semaphore 0)])
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
        (bye)))))

;  site<%>
;  [#:current-directory path-string]
;  [#:htdocs-paths      (listof path-string)]
;  [#:mime-types-path   path-string]
;  [#:404-handler       (-> response)]
; ->
;  (connection request -> response)
(define (make-top-dispatcher
         site
         #:test-suite        [test-suite                #f]
         #:current-directory [servlet-current-directory (current-directory)]
         #:htdocs-paths      [htdocs-paths              (list smoke-htdocs-path)]
         #:mime-types-path   [mime-types-path           smoke-mime-types-path]
         #:404-handler       [404-handler               smoke-404-handler])
  
  ; connection request -> void
  (apply sequencer:make
         `(,@(if test-suite
                 (list (dispatch/servlet
                        (lambda (request)
                          (debug "uri" (url->string (request-uri request)))
                          (run-delirium request test-suite))
                        #:regexp            #rx"^/test"
                        #:current-directory servlet-current-directory
                        #:namespace         '((file "../main.ss"))))
                 null)
           ,(site:make site #:error-handler smoke-500-handler)
           ,@(for/list ([path (in-list `(,@htdocs-paths ,delirium-htdocs-path))])
               (files:make #:url->path       (fsmap:make-url->path path)
                           #:path->mime-type (make-path->mime-type mime-types-path)
                           #:indices         (list "index.html" "index.htm")))
           ,(proc:make 404-handler #:error-handler smoke-500-handler))))

; -> response
(define (smoke-404-handler)
  (make-html-response
   #:code    404
   #:message "Not found"
   (xml (html (head (title "404 not found"))
              (body (p "Sorry! We could not find that file or resource on our server:")
                    (blockquote (tt ,(url->string (request-uri (current-request))))))))))

; exn -> response
(define (smoke-500-handler exn)
  (log-error* (exn-message exn))
  ((error-display-handler) (exn-message exn) exn)
  (make-html-response
   #:code    500
   #:message "Internal error"
   (xml (html (head (title "500 internal error"))
              (body (p "Sorry! Something went wrong there!"))))))

; Provides ---------------------------------------

(provide/contract
 [serve/smoke          (->* ((is-a?/c site<%>))
                            (#:port natural-number/c
                                    #:listen-ip       (or/c string? #f)
                                    #:htdocs-paths    (listof path?)
                                    #:mime-types-path path?
                                    #:launch-browser? boolean?
                                    #:404-handler     (-> response/c))
                            void?)]
 [serve/smoke/delirium  (->* ((is-a?/c site<%>) schemeunit-test?)
                             (#:run-tests (-> any/c any)
                                          #:port            natural-number/c
                                          #:listen-ip       (or/c string? #f)
                                          #:htdocs-paths    (listof path?)
                                          #:mime-types-path path?
                                          #:launch-browser? boolean?
                                          #:404-handler     (-> request? any))
                             void?)])
