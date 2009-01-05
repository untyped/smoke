#lang scheme/base

(require scheme/match
         scheme/contract
         web-server/managers/manager
         web-server/servlet/servlet-structs
         (planet untyped/unlib:3/debug))

;; Utility
(define (make-counter)
  (define i 0)
  (lambda ()
    (set! i (add1 i))
    i))

;[create-instance ((-> void) . -> . number?)]
;[adjust-timeout! (number? number? . -> . void)]
;[clear-continuations! (number? . -> . void)]
;[continuation-store! (number? any/c expiration-handler? . -> . (list/c number? number?))]
;[continuation-lookup (number? number? number? . -> . any/c)]
(define-struct (LRU-manager manager) 
  (initial-life-points
   instance-expiration-handler
   ; Private
   instances
   instance-count-continuations
   next-instance-id
   continuation-life-points
   continuation-reset-life-points!))

(define-struct instance (k-table))

(define-struct k-table (next-id-fn htable))

(define (create-k-table)
  (make-k-table (make-counter) (make-hasheq)))

;; LRU-manager integer -> (listof integer)
(define (lru-life-point-distribution manager num-buckets)
  (define accum (make-vector num-buckets 0))
  (define initial-points (LRU-manager-initial-life-points manager))
  (hash-for-each
   (LRU-manager-instances manager)
   (lambda (instance-id instance)
     (hash-for-each
      (k-table-htable (instance-k-table instance))
      (lambda (k-id k-record)
        (match k-record
          [(list salt k expiration-handler count)
           (let ([index (min (floor (* (/ count initial-points) num-buckets)) (sub1 num-buckets))])
             (vector-set! accum index (add1 (vector-ref accum index))))])))))
  (vector->list accum))

(define (create-LRU-manager
         instance-expiration-handler
         check-interval collect-interval
         collect?
         #:initial-count [initial-count 1]
         #:inform-p [inform-p (lambda _ (void))])
  (define lock (make-semaphore 1))
  ;; Instances
  (define instances (make-hasheq))
  (define next-instance-id (make-counter))    
  
  (define (create-instance expire-fn)
    (define instance-id (next-instance-id))
    (hash-set! instances
               instance-id
               (make-instance (create-k-table)))
    ;; There is a race condition between the two calls
    ;; (create-instance and continuation-store!) necessary to
    ;; create a continuation in a new instance and collect.
    ;; To work around this we add an empty continuation that
    ;; prevents the instance being collected too early.
    (continuation-store! instance-id #f #f)
    instance-id)
  (define (adjust-timeout! instance-id secs)
    (void))
  
  (define (instance-lookup instance-id)
    (define instance
      (hash-ref instances instance-id
                (lambda ()
                  (raise (make-exn:fail:servlet-manager:no-instance
                          (format "No instance for id: ~a" instance-id)
                          (current-continuation-marks)
                          instance-expiration-handler)))))
    instance)
  
  ;; : (instance number number -> (list number continuation expiration-handler? number))
  (define (continuation-record-lookup an-instance a-k-id a-salt)
    (match an-instance
      [(struct instance ((struct k-table (next-id-fn htable))))
       (match (hash-ref htable a-k-id
                        (lambda ()
                          (raise (make-exn:fail:servlet-manager:no-continuation
                                  (format "No continuation for id: ~a" a-k-id)
                                  (current-continuation-marks)
                                  instance-expiration-handler))))
         [(and k-record (list salt k expiration-handler count))
          (if (or (not (eq? salt a-salt))
                  (not k))
              (raise (make-exn:fail:servlet-manager:no-continuation
                      (format "No continuation for id: ~a" a-k-id)
                      (current-continuation-marks)
                      (if expiration-handler
                          expiration-handler
                          instance-expiration-handler)))
              k-record)])]))
  
  ;; : instance-id -> number
  (define (instance-count-continuations instance-id)
    (match (instance-lookup instance-id)
      [(struct instance ((and k-table (struct k-table (next-id-fn htable)))))
       (hash-count htable)]))
  
  ;; Interface
  (define (clear-continuations! instance-id)
    (match (instance-lookup instance-id)
      [(struct instance ((and k-table (struct k-table (next-id-fn htable)))))
       (hash-for-each
        htable
        (match-lambda*
          [(list k-id (list salt k expiration-handler count))
           (hash-set! htable k-id
                      (list salt #f expiration-handler count))]))]))
  
  (define (continuation-store! instance-id k expiration-handler)
    (match (instance-lookup instance-id)
      [(struct instance ((struct k-table (next-id-fn htable))))
       (define k-id (next-id-fn))
       (define salt (random 100000000))
       (hash-set! htable
                  k-id
                  (list salt k expiration-handler initial-count))
       (list k-id salt)]))
  
  (define (continuation-lookup instance-id a-k-id a-salt)
    (match (instance-lookup instance-id)
      [(and instance (struct instance ((struct k-table (next-id-fn htable)))))
       (match (continuation-record-lookup instance a-k-id a-salt)
         [(list salt k expiration-handler count)
          (hash-set! htable a-k-id
                     (list salt k expiration-handler initial-count))
          k])]))
  
  (define (continuation-life-points instance-id a-k-id a-salt)
    (match (continuation-record-lookup (instance-lookup instance-id) a-k-id a-salt)
      [(list salt k expiration-handler count) count]))
  
  (define (continuation-reset-life-points! instance-id a-k-id a-salt)
    (match (instance-lookup instance-id)
      [(and instance (struct instance ((struct k-table (next-id-fn htable)))))
       (match (continuation-record-lookup instance a-k-id a-salt)
         [(list salt k expiration-handler count)
          (hash-set! htable a-k-id
                     (list salt k expiration-handler initial-count))])]))
  
  (define (wrap f)
    (lambda args
      (call-with-semaphore lock (lambda () (apply f args)))))
  
  (define the-manager
    (make-LRU-manager (wrap create-instance)
                      adjust-timeout!
                      (wrap clear-continuations!)
                      (wrap continuation-store!)
                      (wrap continuation-lookup)
                      initial-count
                      ; Specific
                      instance-expiration-handler
                      ; Private
                      instances
                      instance-count-continuations
                      next-instance-id
                      continuation-life-points
                      continuation-reset-life-points!))
  
  ; Collector
  ;;
  ;; points is the number of points to deduct from the count
  (define (collect just-go? [points 1])
    (call-with-semaphore
     lock
     (lambda ()
       (define removed (box 0))
       (hash-for-each
        instances
        (match-lambda*
          [(list instance-id (struct instance ((struct k-table (next-id-fn htable)))))
           (define empty? (box #t))
           (hash-for-each
            htable
            (match-lambda*
              [(list k-id (list s k eh count))
               (if (zero? count)
                   (begin (set-box! removed (add1 (unbox removed)))
                          (hash-remove! htable k-id))
                   (begin (set-box! empty? #f)
                          (hash-set! htable k-id
                                     (list s k eh (max (- count points) 0)))))]))
           (when (unbox empty?)
             (set-box! removed (add1 (unbox removed)))
             (hash-remove! instances instance-id))]))
       (when (or just-go?
                 (not (zero? (unbox removed))))
         (inform-p (unbox removed))
         (collect-garbage)
         (collect-garbage)))))
  
  (define manager-thread
    (thread
     (lambda ()
       (define (seconds->msecs s)
         (+ (current-inexact-milliseconds)
            (* s 1000)))
       (let loop ([msecs0 (seconds->msecs check-interval)]
                  [msecs1 (seconds->msecs collect-interval)])
         (sync (handle-evt
                (alarm-evt msecs0)
                (lambda (_)
                  (let ([points (collect?)])
                    (cond [(integer? points) (if (zero? points)
                                                 (void)
                                                 (collect #f points))]
                          [points            (collect #f)]
                          [else              (void)]))
                  (loop (seconds->msecs check-interval) msecs1)))
               (handle-evt
                (alarm-evt msecs1)
                (lambda _
                  (collect #t)
                  (loop msecs0 (seconds->msecs collect-interval)))))))))
  
  the-manager)

; Provide statements -----------------------------

(provide (struct-out LRU-manager)
         lru-life-point-distribution)

(provide/contract
 [create-LRU-manager (->* (expiration-handler/c number? number? (-> (or/c number? boolean?)))
                          (#:initial-count number? #:inform-p (-> number? void?))
                          manager?)])
