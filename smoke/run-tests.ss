#lang scheme/base

(require (planet untyped/unlib:3/require))

(require web-server/servlet-env
         (planet untyped/delirium:2)
         "all-smoke-tests.ss"
         "smoke.ss"
         "testapp/content-base.ss"
         (directory-in "testapp/content"))

; Main program body ----------------------------

(print-struct #t)
(current-deployment-mode 'test)
(run-smoke/delirium #:start       (lambda ()
                                    (parameterize ([dispatch-url-cleaner smoke-url-cleaner])
                                      (dispatch (current-request) test-site)))
                    #:tests           all-smoke-tests
                    #:htdocs-path     (list testapp-htdocs-path)
                    #:launch-browser? #t)
