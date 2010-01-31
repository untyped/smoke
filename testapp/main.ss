#lang scheme/base

(require "content-base.ss"
         "content/counter.ss")

(serve/smoke test-site #:htdocs-paths (list testapp-htdocs-path))
