#lang scheme/base

(require "../test-base.ss")

(require/expose "web-cell.ss" (frame-namespace))

; Tests ----------------------------------------

(define web-cell-tests
  (test-suite "web-cell.ss"
    
    (test-case "call-with-frame"
      (let ([frame (push-frame)])
        (check-false (equal? (current-frame) frame))
        (call-with-frame frame
          (cut check-equal? (current-frame) frame))
        (check-false (equal? (current-frame) frame))))
    
    (test-case "push-frame"
      (let ([frame1 (current-frame)]
            [frame2 (push-frame)]
            [frame3 (push-frame)])
        (check-eq? (frame-parent frame2) frame1 "frame2")
        (check-eq? (frame-parent frame3) frame1 "frame3")
        (call-with-frame frame2
          (lambda ()
            (let ([frame4 (push-frame)])
              (check-equal? (frame-parent frame4) frame2))))))
    
    (test-case "make-web-cell : initial value"
      (let ([cell (make-web-cell 123)])
        (check-equal? (web-cell-ref cell) 123)))
    
    (test-case "web-cell-set! : value changed"
      (let ([cell (make-web-cell 123)])
        (check-equal? (web-cell-ref cell) 123)
        (web-cell-set! cell 321)
        (check-equal? (web-cell-ref cell) 321)))
    
    (test-case "web-cell-set! : value changed to #f"
      (let ([cell (make-web-cell #f)])
        (check-equal? (web-cell-ref cell) #f)))
    
    (test-case "frame-set?"
      (let ([cell1 (make-web-cell #f)]
            [cell2 (make-web-cell #t)])
        (check-equal? (frame-set? (current-frame) (web-cell-id cell1)) #t "check 1")
        (check-equal? (frame-set? (current-frame) (web-cell-id cell2)) #t "check 2")))
    
    (test-case "web-cell-ref : scoping obeyed"
      (let ([cell (make-web-cell 123)])
        (check-equal? (web-cell-ref cell) 123)
        (call-with-frame (push-frame)
          (lambda ()
            (web-cell-set! cell 321)
            (check-equal? (web-cell-ref cell) 321)))
        (check-equal? (web-cell-ref cell) 123)))
    
    (test-case "web-cell-ref : undefined when initial frame out of scope"
      (let ([cell (call-with-frame (push-frame) (lambda () (make-web-cell 123)))])
        (check-exn exn:fail? (cut web-cell-ref cell))))
    
    (test-case "web-cell-unset!"
      (let ([cell (make-web-cell 123)])
        (check-true (web-cell-set? cell))
        (check-equal? (web-cell-ref cell) 123)
        (web-cell-unset! cell)
        (check-false (web-cell-set? cell))
        (check-exn exn:fail? (cut web-cell-ref cell)))
      (let* ([frame1 (current-frame)]
             [frame2 (push-frame frame1)]
             [frame3 (push-frame frame2)]
             [cell   (make-web-cell 123)])
        (web-cell-set! cell 234 frame3)
        (check-equal? (web-cell-ref cell frame3) 234)
        (web-cell-unset! cell frame3)
        (check-equal? (web-cell-ref cell frame3) 123)))
    
    ; Memory leaks in the root frame -----------
    
    (test-case "make-web-cell : no memory leaks"
      (let ([cell-ids (let loop ([i 100])
                        (if (zero? i)
                            null
                            (cons (call-with-frame (push-frame)
                                    (lambda ()
                                      (web-cell-id (make-web-cell 12345))))
                                  (loop (sub1 i)))))]
            [root-ids (namespace-mapped-symbols (frame-namespace (current-frame)))])
        (with-check-info (['cell-ids cell-ids]
                          ['root-ids root-ids])
          (for-each (lambda (id) (check-false (member id root-ids)))
                    cell-ids))))
    
    ; frame-squeeze! ---------------------------
    
    (test-case "frame-squeeze!"
      (let* ([frame1 (push-frame)]
             [frame2 (call-with-frame frame1 push-frame)])
        (call-with-frame frame1
          (lambda ()
            (let ([cell1 (make-web-cell 1)]
                  [cell2 (make-web-cell 2)]
                  [cell3 (make-web-cell 3)]
                  [cell4 (make-web-cell 4)])
              (check-equal? (frame-ref frame1 (web-cell-id cell1)) 1 "check 1")
              (check-equal? (frame-ref frame1 (web-cell-id cell2)) 2 "check 2")
              (check-equal? (frame-ref frame1 (web-cell-id cell3)) 3 "check 3")
              (check-equal? (frame-ref frame1 (web-cell-id cell4)) 4 "check 4")
              (call-with-frame frame2
                (lambda ()
                  (web-cell-set! cell1 5)
                  (web-cell-set! cell2 6)
                  (check-equal? (frame-ref frame1 (web-cell-id cell1)) 1 "check 5")
                  (check-equal? (frame-ref frame1 (web-cell-id cell2)) 2 "check 6")
                  (check-equal? (frame-ref frame1 (web-cell-id cell3)) 3 "check 7")
                  (check-equal? (frame-ref frame1 (web-cell-id cell4)) 4 "check 8")
                  (check-equal? (frame-ref frame2 (web-cell-id cell1)) 5 "check 9")
                  (check-equal? (frame-ref frame2 (web-cell-id cell2)) 6 "check 10")
                  (check-false (frame-set? frame2 (web-cell-id cell3)) "check 11")
                  (check-false (frame-set? frame2 (web-cell-id cell4)) "check 12")
                  (check-not-exn (cut frame-squeeze! frame2) "check X")
                  (check-equal? (frame-ref frame1 (web-cell-id cell1)) 5 "check 13")
                  (check-equal? (frame-ref frame1 (web-cell-id cell2)) 6 "check 14")
                  (check-equal? (frame-ref frame1 (web-cell-id cell3)) 3 "check 15")
                  (check-equal? (frame-ref frame1 (web-cell-id cell4)) 4 "check 16")
                  (check-false (frame-set? frame2 (web-cell-id cell1)) "check 17")
                  (check-false (frame-set? frame2 (web-cell-id cell2)) "check 18")
                  (check-false (frame-set? frame2 (web-cell-id cell3)) "check 19")
                  (check-false (frame-set? frame2 (web-cell-id cell4)) "check 20"))))))))
    
    ; web-cell-changed? ------------------------
    
    (test-case "web-cell-changed? : undefined cell"
      (let ([cell (call-with-frame (push-frame)
                    (cut make-web-cell 123))])
        (check-exn exn:fail? (lambda () (web-cell-changed? cell)))))
    
    (test-case "web-cell-changed? : current-frame is root frame"
      (let ([cell (make-web-cell 123)])
        (check-true (web-cell-changed? cell))))
    
    (test-case "web-cell-changed? : cell unchanged"
      (let ([cell (make-web-cell 123)])
        (call-with-frame (push-frame)
          (lambda ()
            (check-false (web-cell-changed? cell))
            (call-with-frame 
                (push-frame)
              (lambda ()
                (check-false (web-cell-changed? cell))))))))
    
    (test-case "web-cell-changed? : cell set to different value"
      (let ([cell (make-web-cell 123)])
        (call-with-frame (push-frame)
          (lambda ()
            (web-cell-set! cell 321)
            (check-true (web-cell-changed? cell))))))
    
    (test-case "web-cell-changed? : cell set to same value"
      (let ([cell (make-web-cell 123)])
        (call-with-frame 
            (push-frame)
          (lambda ()
            (web-cell-set! cell 123)
            (check-false (web-cell-changed? cell))))))
    
    (test-case "web-cell-changed? : cell set to #f"
      (let ([cell (make-web-cell #f)])
        (call-with-frame 
            (push-frame)
          (lambda ()
            (web-cell-set! cell #t)
            (check-true (web-cell-changed? cell))
            (call-with-frame 
                (push-frame)
              (lambda ()
                (web-cell-set! cell #f)
                (check-true (web-cell-changed? cell))))))))
    
    (test-case "in-frames"
      (let* ([frame1 (current-frame)]
             [frame2 (push-frame frame1)]
             [frame3 (push-frame frame2)])
        (check-equal? (for/list ([frame (in-frames frame3)])
                        frame)
                      (list frame3 frame2 frame1))))))

; Provide statements --------------------------- 

(provide web-cell-tests)
