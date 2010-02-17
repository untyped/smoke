#lang scheme/base

(require "content-base.ss"
         "content/content.ss")

(dev? #f)

(serve/smoke
 test-site
 #:htdocs-paths (list testapp-htdocs-path))

