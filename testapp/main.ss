#lang scheme/base

(require "content-base.ss"
         "content/counter.ss")

(serve/smoke #:htdocs-paths (list testapp-htdocs-path))
