#lang scheme/base

(require (planet untyped/unlib:3/require))

(require net/url
         (except-in web-server/http redirect-to)
         web-server/servlet-env
         (planet untyped/delirium:3)
         "all-smoke-tests.ss"
         "main.ss"
         "testapp/content-base.ss"
         "testapp/content/content.ss")

; Main program body ----------------------------

(print-struct #t)
(dev? #t)

(thread
 (lambda ()
   (let ([start-time (current-inexact-milliseconds)])
     (let loop ()
       (collect-garbage)
       (collect-garbage)
       (printf "~s\t~s~n"
               (floor (- (current-inexact-milliseconds) start-time))
               (current-memory-use))
       (sleep 1)
       (loop)))))

; We don't need logging output, but we want to check these logging hooks don't exn:
(response-time-logger (lambda (msg url time) (void)))
(frame-size-logger (lambda (url time) (void)))

(serve/smoke/delirium
 test-site
 all-smoke-tests
 #:launch-browser? #t
 #:htdocs-paths (list testapp-htdocs-path))
