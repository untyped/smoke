#lang scheme

(require "../test-base.ss")

(require "callback-url-test.ss"
         "web-cell-test.ss")

(define/provide-test-suite all-core-tests
  callback-url-tests
  web-cell-tests)