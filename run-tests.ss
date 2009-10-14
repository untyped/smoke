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

(serve/smoke/delirium
 (lambda ()
   (dispatch (current-request) test-site))
 all-smoke-tests
 #:launch-browser? #t
 #:htdocs-paths    (list testapp-htdocs-path))
