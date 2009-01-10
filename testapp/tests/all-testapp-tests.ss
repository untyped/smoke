#lang scheme/base

(require "../../test-base.ss"
         "autocomplete-test.ss"
         "counter-test.ss"
         "current-request-test.ss"
         "editor-test.ss"
         "form-hidden-test.ss"
         "form-test.ss"
         "redirect-test.ss"
         "requirements-test.ss"
         "segfault-test.ss"
         "tab-test.ss"
         "tooltip-test.ss")

; Tests ------------------------------------------

(define all-testapp-tests
  (test-suite "testapp"
    counter-tests
    current-request-tests
    requirements-tests
    redirect-tests
    editor-tests
    form-tests
    form-tests/hidden
    tab-tests
    segfault-tests
    autocomplete-tests
    tooltip-tests))

; Provide statements -----------------------------

(provide all-testapp-tests)
