#lang scheme/base

(require "content-base.ss"
         "content/counter.ss")

(serve/smoke
 (new application% [page counter-page])
 #:htdocs-paths (list testapp-htdocs-path))
