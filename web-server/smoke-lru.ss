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
(define (make-default-smoke-manager
         ; We start docking life points at 20% of 128M:
         #:memory-threshold [threshold        (* 128 1024 1024)]
         ; The memory use condition is checked every 5 seconds:
         #:check-interval   [check-interval   5000]
         ; One life point is deducted naturally every minute:
         #:natural-interval [natural-interval 60000]
         ; Start with 300 life points (natural-lifetime = initial-points * natural-interval = 300 mins):
         #:initial-points   [initial-points   300]
         ; Log diagnostic information every 5 seconds:
         #:message-interval [message-interval 5000])
  (letrec ([next-message (+ (current-inexact-milliseconds) message-interval)]
           [threshold1   (* threshold 1.00)]
           [threshold2   (* threshold 0.80)]
           [threshold3   (* threshold 0.60)]
           [threshold4   (* threshold 0.40)]
           [threshold5   (* threshold 0.20)] ; in bytes
           [manager      (create-LRU-manager
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
                          (quotient check-interval 1000)
                          (quotient natural-interval 1000)
                          ; Detemine the number of life points to deduct from the continuation:
                          (lambda ()
                            (let* ([memory-use   (current-memory-use)]
                                   [collect-rate (cond [(> memory-use threshold1) 50]
                                                       [(> memory-use threshold2) 10]
                                                       [(> memory-use threshold3)  5]
                                                       [(> memory-use threshold4)  3]
                                                       [(> memory-use threshold5)  1]
                                                       [else #f])]
                                   [now          (current-inexact-milliseconds)])
                              ; Log collection rate and memory stats:
                              (when (> now next-message)
                                (collect-garbage)
                                (set! next-message (+ now message-interval))
                                (log-info* "Memory use" memory-use
                                           "threshold"  threshold
                                           "rate"       collect-rate
                                           "detail"     (lru-life-point-distribution manager 10)))
                              ; Return collection rate:
                              collect-rate))
                          #:initial-count initial-points
                          ; Log when continuations are collected:
                          #:inform-p
                          (lambda args
                            (unless (and (pair? args) (integer? (car args)) (zero? (car args)))
                              (log-info* "Collected" args))
                            (void)))])
    manager))

; Provide statements -----------------------------

(provide/contract
 [make-default-smoke-manager
  (->* () (#:memory-threshold natural-number/c
                              #:check-interval   (and/c integer? (>=/c 1000))
                              #:natural-interval (and/c integer? (>=/c 1000))
                              #:initial-points   (and/c integer? (>=/c 1))
                              #:message-interval (and/c integer? (>=/c 1000)))
       manager?)])
