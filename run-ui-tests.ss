#lang scheme/base

(require (planet untyped/unlib:3/require))

(require net/url
         (except-in web-server/http redirect-to)
         web-server/servlet-env
         (planet untyped/delirium:3)
         "all-smoke-tests.ss"
         "smoke.ss"
         (except-in "testapp/content-base.ss" focus)
         "testapp/content/content.ss"
         "testapp/tests/all-testapp-tests.ss")

; Main program body ----------------------------

(print-struct #t)
(dev? #t)

; We don't need logging output, but we want to check these logging hooks don't exn:
(response-time-logger (lambda (msg url time) (void)))
(frame-size-logger (lambda (url time) (void)))

(serve/smoke/delirium
 (lambda ()
   (site-dispatch test-site (current-request)))
 all-testapp-tests
 #:launch-browser? #t
 #:htdocs-paths    (list testapp-htdocs-path)
 #:manager         (make-default-smoke-manager
                    #:message-interval 5000
                    #:message-logger   (lambda (use threshold purge rate span)
                                         (void))))