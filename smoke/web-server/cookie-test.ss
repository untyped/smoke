#lang scheme/base

(require srfi/26/cut
         "../test-base.ss"
         "cookie.ss")

; Helpers ----------------------------------------

; string -> boolean
(define (rfc-date? str)
  (regexp-match
   #rx"[A-Z][a-z][a-z], [0-9][0-9]-[A-Z][a-z][a-z]-[0-9][0-9][0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9] GMT"
   str))

; Tests ------------------------------------------

(define cookie-tests
  (test-suite "All tests for cookie"
    
    (test-case "rfc822 string generated correctly"
      (for-each
       (cut check-pred rfc-date? <>)
       (map expires->rfc822-string
            `(0 1 2 100 123456 ,(current-seconds)))))
    
    (test-case "print-cookie includes expires attribute"
      (check-pred
       (cut regexp-match "expires=" <>)
       (print-cookie
        (cookie:add-expires
         (set-cookie "foo" "bar")
         10))))
    
    (test-case "print-cookie formats cookie correctly"
      (check
       string=?
       (print-cookie
        (cookie:add-expires
         (cookie:add-domain
          (cookie:add-path
           (set-cookie "foo;," "bar;,")
           "/")
          ".untyped.com")
         10))
       (string-append
        "foo#3B#2C=bar#3B#2C; expires="
        (expires->rfc822-string 10)
        "; path=/; domain=.untyped.com")))
    
    (test-equal? "get-cookie/single"
      (get-cookie/single "foo;,"
                         (print-cookie
                          (cookie:add-expires
                           (cookie:add-domain
                            (cookie:add-path
                             (set-cookie "foo;," "bar;,")
                             "/")
                            ".untyped.com")
                           10)))
      "bar;,")))

; Provide statements -----------------------------

(provide cookie-tests)
