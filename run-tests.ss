#lang scheme/base

(require (planet untyped/unlib:3/require))

(require net/url
         (except-in web-server/http redirect-to)
         web-server/servlet-env
         (planet untyped/delirium:3)
         "all-smoke-tests.ss"
         "smoke.ss"
         (except-in "testapp/content-base.ss" focus)
         "testapp/content/content.ss")

; Main program body ----------------------------

(print-struct #t)
(dev? #t)

; We don't need logging output, but we want to check these logging hooks don't exn:
(response-time-logger void)
(frame-size-logger void)

(serve/smoke/delirium
 (lambda ()
   (site-dispatch test-site (current-request)))
 all-smoke-tests
 #:launch-browser? #t
 #:htdocs-paths    (list testapp-htdocs-path))
