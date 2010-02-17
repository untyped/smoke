#lang scheme/base

(require "../../test-base.ss"
         "../../core/session.ss"
         "../base.ss")

; Tests ------------------------------------------

(define session-tests
  (test-suite "session.ss"
    
    (test-case "session started automatically"
      (open/wait (controller-url session-page))
      (check-true (node-exists? (node/id 'cookie-id)))
      (check-not-false (regexp-match #px"[a-z0-9]{32}" (inner-html-ref (node/id 'cookie-id)))))
    
    (test-case "session-set!"
      (open/wait (controller-url session-set-page 'key1 "val1"))
      (check-true (node-exists? (node/jquery "th:contains('key1') + td:contains('\"val1\"')")))
      (open/wait (controller-url session-set-page 'key2 "val2"))
      (check-true (node-exists? (node/jquery "th:contains('key1') + td:contains('\"val1\"')")))
      (check-true (node-exists? (node/jquery "th:contains('key2') + td:contains('\"val2\"')")))
      (open/wait (controller-url session-set-page 'key3 "val3"))
      (check-true (node-exists? (node/jquery "th:contains('key1') + td:contains('\"val1\"')")))
      (check-true (node-exists? (node/jquery "th:contains('key2') + td:contains('\"val2\"')")))
      (check-true (node-exists? (node/jquery "th:contains('key3') + td:contains('\"val3\"')"))))
    
    (test-case "session-remove!"
      (open/wait (controller-url session-remove-page 'key2))
      (check-true (node-exists? (node/jquery "th:contains('key1') + td:contains('\"val1\"')")))
      (check-true (node-exists? (node/jquery "th:contains('key3') + td:contains('\"val3\"')"))))
    
    #;(test-case "end-session"
        (let ([old-cookie-id (inner-html-ref (node/id 'cookie-id))])
          (open/wait (controller-url session-end-page))
          (open/wait (controller-url session-page))
          (check-false (equal? old-cookie-id (inner-html-ref (node/id 'cookie-id))))
          (check-false (node-exists? (node/jquery "tbody th + td")))))))

; Provide statements -----------------------------

(provide session-tests)

