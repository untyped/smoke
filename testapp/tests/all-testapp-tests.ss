#lang web-server

(require "../../test-base.ss"
         "autocomplete-test.ss"
         "counter-test.ss"
         "current-request-test.ss"
         "editor-test.ss"
         "focus-test.ss"
         "form-hidden-test.ss"
         "form-test.ss"
         "notification-test.ss"
         "redirect-test.ss"
         "requirements-test.ss"
         "segfault-test.ss"
         "session-test.ss"
         "tab-test.ss"
         "tooltip-test.ss")

; Tests ------------------------------------------

(define all-testapp-tests
  (test-suite "testapp"
    session-tests
    counter-tests
    current-request-tests
    requirements-tests
    redirect-tests
    notification-tests
    focus-tests
    form-tests
    form-tests/hidden
    tab-tests
    segfault-tests
    autocomplete-tests
    tooltip-tests))

; Provide statements -----------------------------

(provide all-testapp-tests)
