#lang web-server

(require (planet untyped/unlib:3/require))

(require scheme/cmdline
         "content-base.ss"
         "content/content.ss")

(serve/smoke
 (lambda ()
   (site-dispatch test-site (current-request)))
 #:htdocs-paths (list testapp-htdocs-path))
