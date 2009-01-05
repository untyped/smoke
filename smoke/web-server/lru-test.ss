#lang scheme/base

(require scheme/match
         scheme/pretty
         (except-in web-server/managers/manager manager)
         "../test-base.ss"
         "lru.ss")

; Helpers ----------------------------------------

(define memory-threshold
  (* 1024 1024 1024))

(define channel (make-channel))

(define (collect-points points)
  (channel-put channel points)
  (channel-get channel))

(define manager
  (create-LRU-manager
   ; Called when an instance has expired.
   void
   ; The condition below is checked every second
   1
   ; One 'life point' is deducted every 30 minutes
   (* 30 60)
   ; If this condition returns an integer,
   ; that many life points are deducted from the continuations.
   (lambda () 
     (let ([points (channel-try-get channel)])
       (if points
           (begin0 points
                   (sleep 1)
                   (channel-put channel 'boo-yah))
           #f)))
   ; The number of 'life points' an continuation starts with
   #:initial-count 12
   ; Logging done whenever an continuation is collected
   #:inform-p void))

(define instance-id ((manager-create-instance manager) void))

(define k (lambda _ "Hello!"))

; Tests -------------------------------------------

(define lru-tests
  (test-suite "lru.ss"
    
    (test-case "create-instance"
      (check-equal? instance-id 1))
    
    (test-case "create-instance adds empty continuation"
      (let ([id ((manager-create-instance manager) void)])
        (check-equal? ((LRU-manager-instance-count-continuations manager) id)
                      1)))
    
    (test-case "instance-count-continuations correct"
      (let ([id ((manager-create-instance manager) void)])
        ((manager-continuation-store! manager) id k #f)
        ((manager-continuation-store! manager) id k #f)
        ((manager-continuation-store! manager) id k #f)
        ((manager-continuation-store! manager) id k #f)
        (check-equal? ((LRU-manager-instance-count-continuations manager) id)
                      5)))

    (test-case "continuation-store!"
      (match-let ([(list k-id salt) ((manager-continuation-store! manager) instance-id k #f)])
        (check-equal? k-id 2 "k-id")
        (check-pred integer? salt "salt")))
    
    (test-case "continuation-lookup"
      (match-let ([(list k-id salt) ((manager-continuation-store! manager) instance-id k #f)])
        (check-equal? ((manager-continuation-lookup manager) instance-id k-id salt) k)))
    
    (test-case "continuation-lookup resets life points"
      (match-let ([(list k-id salt) ((manager-continuation-store! manager) instance-id k #f)])
        (collect-points 4)
        (check-equal? ((LRU-manager-continuation-life-points manager) instance-id k-id salt) 8)
        (check-equal? ((manager-continuation-lookup manager) instance-id k-id salt) k)     
        (check-equal? ((LRU-manager-continuation-life-points manager) instance-id k-id salt) 12)))
    
    
    (test-case "continuation-life-points"
      (match-let ([(list k-id salt) ((manager-continuation-store! manager) instance-id k #f)])
        (check-equal? ((LRU-manager-continuation-life-points manager) instance-id k-id salt) 12)))
    
    (test-case "continuation-life-points w/ non-existent instance"
      (check-exn exn:fail:servlet-manager:no-instance?
        (lambda () ((LRU-manager-continuation-life-points manager) 12312 123123 1))))
    
    (test-case "continuation-life-points w/ non-existent continuation"
      (check-exn exn:fail:servlet-manager:no-continuation?
        (lambda () ((LRU-manager-continuation-life-points manager) instance-id 123123 1))))
    
    (test-case "continuation-life-points w/ incorrect salt"
      (check-exn exn:fail:servlet-manager:no-continuation?
        (lambda ()
          (match-let ([(list k-id salt) ((manager-continuation-store! manager) instance-id k #f)])
            ((LRU-manager-continuation-life-points manager) instance-id k-id (sub1 salt))))))
    
    (test-case "life points are deducted by the given amount"
      (match-let ([(list k-id salt) ((manager-continuation-store! manager) instance-id k #f)])
        (check-equal? ((LRU-manager-continuation-life-points manager) instance-id k-id salt) 12)
        (collect-points 4)
        (check-equal? ((LRU-manager-continuation-life-points manager) instance-id k-id salt) 8)))
    
    
    (test-case "continuation-reset-life-points"
      (match-let ([(list k-id salt) ((manager-continuation-store! manager) instance-id k #f)])
        (check-equal? ((LRU-manager-continuation-life-points manager) instance-id k-id salt) 12)
        (collect-points 4)
        (check-equal? ((LRU-manager-continuation-life-points manager) instance-id k-id salt) 8)
        ((LRU-manager-continuation-reset-life-points! manager) instance-id k-id salt)
        (check-equal? ((LRU-manager-continuation-life-points manager) instance-id k-id salt) 12)))
    
    (test-case "continuation-reset-life-points! w/ non-existent instance"
      (check-exn exn:fail:servlet-manager:no-instance?
        (lambda () ((LRU-manager-continuation-reset-life-points! manager) 123 123 1))))
    
    (test-case "continuation-reset-life-points! w/ non-existent continuation"
      (check-exn exn:fail:servlet-manager:no-continuation?
        (lambda () ((LRU-manager-continuation-reset-life-points! manager) instance-id 123123 1))))
    
    (test-case "continuation-reset-life-points! w/ incorrect salt"
      (check-exn exn:fail:servlet-manager:no-continuation?
        (lambda ()
          (match-let ([(list k-id salt) ((manager-continuation-store! manager) instance-id k #f)])
            ((LRU-manager-continuation-reset-life-points! manager) instance-id k-id (sub1 salt))))))
    
    ; Continuations are not collected on the sweep where their points fall to zero, but on the sweep following
    ; Additionally, we must add another continuation to stop the instance being collected
    (test-case "continuations are collected when points are zero"
      (match-let ([(list k-id salt) ((manager-continuation-store! manager) instance-id k #f)])
        (collect-points 12)
        ((manager-continuation-store! manager) instance-id k #f)
        (check-equal? ((LRU-manager-continuation-life-points manager) instance-id k-id salt) 0)
        (collect-points 0)
        (check-exn exn:fail:servlet-manager:no-continuation?
          (lambda ()
            (debug "points" ((LRU-manager-continuation-life-points manager) instance-id k-id salt))))))
    
    (test-case "continuations are collected when points less than zero"
      (match-let ([(list k-id salt) ((manager-continuation-store! manager) instance-id k #f)])
        (collect-points 20)
        ((manager-continuation-store! manager) instance-id k #f)
        (collect-points 0)
        (check-exn exn:fail:servlet-manager:no-continuation?
          (lambda ()
            (debug "points" ((LRU-manager-continuation-life-points manager) instance-id k-id salt))))))))

; Provide statements -----------------------------

(provide lru-tests)
