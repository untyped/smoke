#lang scheme

(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui)
         "web-cell.ss")

(require/expose "web-cell.ss"
  (make-web-frame web-frame-env))

; Tests ----------------------------------------

(define/provide-test-suite web-cell-tests
  
  (test-case "web-cell-ref"
    (let ([cell (make-web-cell 123)])
      (check-equal? (web-cell-ref cell) 123)))
  
  (test-case "web-cell-set!"
    (let ([cell (make-web-cell 123)])
      (web-cell-set! cell 234)
      (check-equal? (web-cell-ref cell) 234)
      (web-cell-set! cell #f)
      (check-equal? (web-cell-ref cell) #f)))
  
  (test-case "web-cell-changed?"
    (let ([cell (make-web-cell 123)])
      (check-false (web-cell-changed? cell))
      (web-cell-set! cell 234)
      (check-true (web-cell-changed? cell))
      (web-cell-set! cell #f)
      (check-true (web-cell-changed? cell))
      (web-cell-set! cell 123)
      (check-false (web-cell-changed? cell))))
  
  (test-case "with-old-web-frame"
    (let ([cell (make-web-cell 123)])
      (check-equal? (with-old-web-frame (web-cell-ref cell)) 123)
      (web-cell-set! cell 234)
      (check-equal? (with-old-web-frame (web-cell-ref cell)) 123)
      (web-cell-set! cell 345)
      (check-equal? (with-old-web-frame (web-cell-ref cell)) 123)))
  
  (test-case "update-web-frame!"
    (update-web-frame! (make-web-frame #hasheq()))
    (check-equal? (web-frame-env (capture-web-frame)) #hasheq()))
  
  (test-case "capture-web-frame"
    (update-web-frame! (make-web-frame #hasheq()))
    (let ([a (make-web-cell 123)]
          [b (make-web-cell 234)])
      (check-equal? (web-frame-env (capture-web-frame)) #hasheq())
      (web-cell-set! a 234)
      (check-equal? (web-frame-env (capture-web-frame)) 
                    (make-immutable-hasheq (list (cons (web-cell-id a) 234))))
      (web-cell-set! b 123)
      (check-equal? (web-frame-env (capture-web-frame)) 
                    (make-immutable-hasheq (list (cons (web-cell-id a) 234)
                                                 (cons (web-cell-id b) 123))))
      (web-cell-set! a 123)
      (check-equal? (web-frame-env (capture-web-frame))
                    (make-immutable-hasheq (list (cons (web-cell-id b) 123))))
      (web-cell-set! b 234)
      (check-equal? (web-frame-env (capture-web-frame)) 
                    (make-immutable-hasheq null)))))
