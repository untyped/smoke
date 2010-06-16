#lang scheme/base

(require "test-base.ss")

(require "all-smoke-tests.ss")

; Main program body ----------------------------

(print-struct #t)
(dev? #t)

(run-tests all-smoke-tests)
