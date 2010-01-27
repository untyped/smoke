#lang web-server

(require "../test-base.ss"
         "continuation-url-test.ss"
         "cookie-test.ss"
         "lru-test.ss"
         "web-cell-test.ss")

; Tests ------------------------------------------

(define all-web-server-tests
  (test-suite "web-server"
    continuation-url-tests
    cookie-tests
    #;lru-tests
    ; Must run web-cell-tests before sending any responses (which messes up the frame stack tests):
    web-cell-tests))

; Provide statements -----------------------------

(provide all-web-server-tests)
