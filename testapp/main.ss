#lang scheme/base

(require (planet untyped/unlib:3/require))

(require scheme/cmdline
         "content-base.ss"
         "content/content.ss")

(serve/smoke
 (lambda ()
   (dispatch (current-request) test-site))
 #:htdocs-paths (list testapp-htdocs-path))
