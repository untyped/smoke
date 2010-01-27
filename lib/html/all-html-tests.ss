#lang web-server

(require "../../test-base.ss"
         "form-element-test.ss"
         "html-component-test.ss"
         "html-element-test.ss"
         "html-page-frame-test.ss"
         "html-page-script-test.ss")

; Tests ------------------------------------------

(define all-html-tests
  (test-suite "html"
    html-component-tests
    html-element-tests
    html-page-frame-tests
    html-page-script-tests
    form-element-tests))

; Provide statements -----------------------------

(provide all-html-tests)
