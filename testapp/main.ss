#lang scheme/base

(require "content-base.ss"
         "content/content.ss")

(serve/smoke test-site
             #:htdocs-paths (list testapp-htdocs-path))
