#lang scheme/base

(require "../../test-base.ss"
         "../../web-server/session.ss"
         "../base.ss")

; Tests ------------------------------------------

(define session-tests
  (test-suite "session.ss"

    (test-case "session started automatically"
      (open/wait (controller-url test-session-show))
      (check-true (node-exists? (node/id 'cookie-id)))
      (check-not-false (regexp-match #px"[a-z0-9]{32}" (inner-html-ref (node/id 'cookie-id)))))
    
    (test-case "session-set!"
      (open/wait (controller-url test-session-set 'key1 "val1"))
      (check-true (node-exists? (node/jquery "th:contains('key1') + td:contains('\"val1\"')")))
      (open/wait (controller-url test-session-set 'key2 "val2"))
      (check-true (node-exists? (node/jquery "th:contains('key1') + td:contains('\"val1\"')")))
      (check-true (node-exists? (node/jquery "th:contains('key2') + td:contains('\"val2\"')")))
      (open/wait (controller-url test-session-set 'key3 "val3"))
      (check-true (node-exists? (node/jquery "th:contains('key1') + td:contains('\"val1\"')")))
      (check-true (node-exists? (node/jquery "th:contains('key2') + td:contains('\"val2\"')")))
      (check-true (node-exists? (node/jquery "th:contains('key3') + td:contains('\"val3\"')"))))
    
    (test-case "session-remove!"
      (open/wait (controller-url test-session-remove 'key2))
      (check-true (node-exists? (node/jquery "th:contains('key1') + td:contains('\"val1\"')")))
      (check-true (node-exists? (node/jquery "th:contains('key3') + td:contains('\"val3\"')"))))
    
    (test-case "end-session"
      (let ([old-cookie-id (inner-html-ref (node/id 'cookie-id))])
        (open/wait (controller-url test-session-end #f))
        (open/wait (controller-url test-session-show))
        (check-false (equal? old-cookie-id (inner-html-ref (node/id 'cookie-id))))
        (check-false (node-exists? (node/jquery "tbody th + td")))))))

; Provide statements -----------------------------

(provide session-tests)

