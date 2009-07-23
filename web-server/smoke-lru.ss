#lang scheme/base

(require scheme/contract
         scheme/pretty
         web-server/managers/manager
         (planet untyped/mirrors:2)
         "../base.ss"
         "continuation-url.ss"
         "expired-continuation.ss"
         "lru.ss"
         "notification.ss"
         "servlet.ss"
         "session-cell.ss")

; [natural] -> manager
(define (make-default-smoke-manager #:memory-threshold [threshold (* 128 1024 1024)])
  (letrec ([message-counter 0]
           [threshold1 (* threshold 1.00)]
           [threshold2 (* threshold 0.80)]
           [threshold3 (* threshold 0.60)]
           [threshold4 (* threshold 0.40)]
           [threshold5 (* threshold 0.20)] ; in bytes
           [manager    (create-LRU-manager
                        ; Called when an instance has expired:
                        (lambda (request)
                          (expired-continuation-type-set!
                           (cond [(ajax-request? request) (expired-continuation-types ajax)]
                                 [(post-request? request) (expired-continuation-types post)]
                                 [else                    (expired-continuation-types get)]))
                          (if (ajax-request? request)
                              (make-js-response 
                               #:code    200
                               #:message "Expired continuation (AJAX response)"
                               (js (= (!dot window location) ,(url->initial-url (request-uri request)))))
                              (make-redirect-response 
                               (url->initial-url (request-uri request))
                               #:code    301
                               #:message "Expired continuation")))
                        ; The condition below is checked every 5 seconds:
                        5
                        ; One 'life point' is deducted every minute:
                        60
                        ; Detemine the number of life points to deduct from the continuation:
                        (lambda ()
                          (define memory-use (current-memory-use))
                          (define collect
                            (cond [(> memory-use threshold1) 50]
                                  [(> memory-use threshold2) 10]
                                  [(> memory-use threshold3)  5]
                                  [(> memory-use threshold4)  3]
                                  [(> memory-use threshold5)  1]
                                  [else #f]))
                          (when (zero? message-counter)
                            (collect-garbage)
                            (log-info* "Memory use" memory-use threshold "rate" collect
                                       "detail" (lru-life-point-distribution manager 10)))
                          (set! message-counter (remainder (add1 message-counter) 12))
                          collect)
                        ; The number of 'life points' an continuation starts with
                        #:initial-count 300
                        ; Logging when continuations are collected
                        #:inform-p
                        (lambda args
                          (unless (and (pair? args) (integer? (car args)) (zero? (car args)))
                            (log-info* "Collected" args))
                          (void)))])
    manager))

; Provide statements -----------------------------

(provide/contract
 [make-default-smoke-manager  (->* () (#:memory-threshold natural-number/c) manager?)])
