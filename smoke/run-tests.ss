#lang scheme/base

(require (planet untyped/unlib:3/require))

(require net/url
         (except-in web-server/http redirect-to)
         web-server/servlet-env
         (planet untyped/delirium:3)
         "all-smoke-tests.ss"
         "smoke.ss"
         "testapp/content-base.ss"
         (directory-in "testapp/content"))

; Main program body ----------------------------

(print-struct #t)

(current-deployment-mode 'test)

(serve/smoke/delirium
 (lambda ()
   (printf "Initial dispatch: ~a~n" (url->string (request-uri (current-request))))
   (parameterize ([dispatch-url-cleaner smoke-url-cleaner])
     (dispatch (current-request) test-site)))
 all-smoke-tests
 #:htdocs-paths (list testapp-htdocs-path))
