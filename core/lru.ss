#lang scheme

(require web-server/http
         web-server/managers/manager
         (planet untyped/mirrors:2)
         "../base.ss"
         "callback.ss"
         "request.ss"
         "lru-internal.ss")

; (box (U natural #f))
(define purge-box
  (box #f))

; natural -> void
(define (purge-continuations! num)
  (set-box! purge-box num))

; [natural] -> manager
(define (make-default-smoke-manager
         ; We start docking life points at 20% of 256M:
         #:memory-threshold  [threshold         (* 256 1024 1024)]
         ; The memory use condition is checked every 5 seconds:
         #:check-interval    [check-interval    5000]
         ; One life point is deducted naturally every minute:
         #:natural-interval  [natural-interval  60000]
         ; Start with 300 life points (natural-lifetime = initial-points * natural-interval = 300 mins):
         #:initial-points    [initial-points    300]
         #:purge-points      [purge-points      100]
         ; Log diagnostic information every 60 seconds:
         #:message-interval  [message-interval  (* 60 1000)]
         ; memory-use:number threshold:number purge-value:number purge-rate:number detail:listof-number -> void
         #:message-logger    [message-logger    #f]
         #:collection-logger [collection-logger #f])
  (letrec ([next-message (+ (current-inexact-milliseconds) message-interval)]
           [initial-use  (let ([use (begin
                                      (collect-garbage)
                                      (collect-garbage)
                                      (current-memory-use))])
                           (printf "Base memory use ~a~n" use)
                           (if (<= threshold use)
                               (error "LRU memory threshold <= initial memory use" (list threshold use))
                               use))]
           [threshold1   (+ initial-use (* (- threshold initial-use) 1.00))]
           [threshold2   (+ initial-use (* (- threshold initial-use) 0.80))]
           [threshold3   (+ initial-use (* (- threshold initial-use) 0.60))]
           [threshold4   (+ initial-use (* (- threshold initial-use) 0.40))]
           [threshold5   (+ initial-use (* (- threshold initial-use) 0.20))] ; in bytes
           [manager      (create-LRU-manager
                          ; Called when an instance has expired:
                          (lambda (request)
                            ;(expired-continuation-type-set!
                            ; (cond [(ajax-request? request) (expired-continuation-types ajax)]
                            ;       [(post-request? request) (expired-continuation-types post)]
                            ;       [else                    (expired-continuation-types get)]))
                            (if (ajax-request? request)
                                (make-js-response 
                                 #:code    200
                                 #:message "Expired continuation (AJAX response)"
                                 (js (= (!dot window location href)
                                        ,(url->initial (request-uri request)))))
                                (make-redirect-response 
                                 (url->initial (request-uri request))
                                 #:code    301
                                 #:message "Expired continuation")))
                          (quotient check-interval   1000)
                          (quotient natural-interval 1000)
                          ; Detemine the number of life points to deduct from the continuation:
                          (lambda ()
                            (let* ([purge        (let ([num (unbox purge-box)])
                                                   (when num (set-box! purge-box #f))
                                                   num)]
                                   [memory-use   (current-memory-use)]
                                   [collect-rate (cond [(> memory-use threshold1) (quotient initial-points 2)]
                                                       [(> memory-use threshold2) 10]
                                                       [(> memory-use threshold3) 5]
                                                       [(> memory-use threshold4) 3]
                                                       [(> memory-use threshold5) 1]
                                                       [else #f])]
                                   [now          (current-inexact-milliseconds)])
                              ; Log collection rate and memory stats:
                              (when (and message-logger (> now next-message))
                                (collect-garbage)
                                (set! next-message (+ now message-interval))
                                (message-logger memory-use
                                                threshold
                                                purge
                                                collect-rate
                                                (lru-life-point-distribution manager 10)))
                              ; Return collection rate:
                              (if purge
                                  (if collect-rate
                                      (lambda (points)
                                        (if (integer? points)
                                            (if (>= points purge)
                                                (- points collect-rate)
                                                0)
                                            #f))
                                      (lambda (points)
                                        (if (integer? points)
                                            (if (>= points purge)
                                                points
                                                0)
                                            #f)))
                                  (if collect-rate
                                      (lambda (points)
                                        (if (integer? points)
                                            (- points collect-rate)
                                            #f))
                                      #f))))
                          #:initial-count initial-points
                          ; Log when continuations are collected:
                          #:inform-p
                          (lambda (num)
                            (when (and num (> num 0))
                              (set-box! purge-box purge-points)
                              (when collection-logger
                                (collection-logger num)))
                            (void)))])
    manager))

; Helpers ----------------------------------------

; LRU-manager integer -> (listof integer)
(define (lru-life-point-distribution manager num-buckets)
  (let ([accum   (make-vector num-buckets 0)]
        [initial (LRU-manager-initial-life-points manager)])
    (for ([(instance-id instance) (in-dict (LRU-manager-instances manager))])
      (for ([(k-id k-record) (in-dict (k-table-htable (instance-k-table instance)))])
        (match k-record
          [(list salt k expiration-handler count)
           (let ([index (min (floor (* (/ count initial) num-buckets)) (sub1 num-buckets))])
             (vector-set! accum index (add1 (vector-ref accum index))))])))
    (vector->list accum)))

; Provide statements -----------------------------

(provide/contract
 [purge-continuations! (-> natural-number/c void?)]
 [make-default-smoke-manager
  (->* () (#:memory-threshold natural-number/c
                              #:check-interval    (and/c integer? (>=/c 1000))
                              #:natural-interval  (and/c integer? (>=/c 1000))
                              #:initial-points    (and/c integer? (>=/c 1))
                              #:purge-points      (or/c natural-number/c #f)
                              #:message-interval  (and/c integer? (>=/c 1000))
                              #:message-logger    (or/c (-> number?
                                                            number?
                                                            (or/c number? #f)
                                                            (or/c number? #f)
                                                            (listof number?)
                                                            any) #f)
                              #:collection-logger (or/c (-> (or/c number? #f) any) #f))
       manager?)])
