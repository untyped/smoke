#lang scheme/base

(require srfi/13/string
         "../../test-base.ss")

; Helpers ----------------------------------------

; -> integer
(define (notification-count)
  (length (inner-html-ref* (node/jquery ".notification"))))

; Tests ------------------------------------------

(define notification-tests
  (test-suite "notification"
    
    (test-case "normal notifications disappear on reload"
      (open/wait "/notification1")
      (check-equal? (notification-count) 0)
      (click/wait (node/id 'normal-2))
      (check-equal? (notification-count) 2)
      (reload/wait)
      (check-equal? (notification-count) 0))
    
    (test-case "sticky notifications disappear on reload"
      (open/wait "/notification1")
      (check-equal? (notification-count) 0)
      (click/wait (node/id 'sticky-2))
      (check-equal? (notification-count) 2))
    
    (test-case "sticky notifications reappear on other pages"
      (open/wait "/notification2")
      (check-equal? (notification-count) 2))
    
    (test-case "notifications dismissed by clicking 'x'"
      (click/wait (node/first (node/jquery ".notification .dismiss")))
      (check-equal? (notification-count) 1)
      (reload/wait)
      (check-equal? (notification-count) 1)
      (click/wait (node/first (node/jquery ".notification .dismiss")))
      (reload/wait)
      (check-equal? (notification-count) 0))))

; Provide statements -----------------------------

(provide notification-tests)

