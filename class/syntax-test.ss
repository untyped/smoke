#lang scheme/base

(require (planet untyped/unlib:3/hash)
         "../test-base.ss"
         "../lib/component.ss"
         "syntax.ss")

; Tests ------------------------------------------

(define syntax-tests
  (test-suite "syntax.ss"
    
    (test-case "class name inferred correctly"
      (check-equal? (format "~a" (let ([test% (class/cells object% ())]) test%)) "#<class:test%>" "let")
      (check-not-false (regexp-match #rx"#<class:.*>" (format "~a" (class/cells object% ()))) "anonymous"))
    
    (test-case "init-cell and init-field: initial values are required"
      (check-exn exn:fail:object?
        (lambda ()
          (singleton/cells component% ()
            (init-field x #:accessor))))
      (check-exn exn:fail:object?
        (lambda ()
          (singleton/cells component% ()
            (init-cell x #:accessor))))
      (check-exn exn:fail:object?
        (lambda ()
          (singleton/cells component% ()
            (init-field x))))
      (check-exn exn:fail:object?
        (lambda ()
          (singleton/cells component% ()
            (init-cell x)))))
    
    (test-case "init-cells and init-fields: initial values are required"
      (check-exn exn:fail:object?
        (lambda ()
          (singleton/cells component% ()
            (init-fields ([x #:accessor])))))
      (check-exn exn:fail:object?
        (lambda ()
          (singleton/cells component% ()
            (init-cells ([x #:accessor])))))
      (check-exn exn:fail:object?
        (lambda ()
          (singleton/cells component% ()
            (init-fields ([x])))))
      (check-exn exn:fail:object?
        (lambda ()
          (singleton/cells component% ()
            (init-cells ([x]))))))
    
    (test-case "cell and field: initial values are correct"
      (let ([obj (singleton/cells component% ()
                   (field [a 1] #:accessor)
                   (field b 2 #:accessor)
                   (cell [c 3] #:accessor)
                   (cell d 4 #:accessor)
                   (field [e 5])
                   (field f 6)
                   (cell [g 7])
                   (cell h 8))])
        (check-equal? (send obj get-a) 1)
        (check-equal? (send obj get-b) 2)
        (check-equal? (send obj get-c) 3)
        (check-equal? (send obj get-d) 4)
        (check-equal? (get-field e obj) 5)
        (check-equal? (get-field f obj) 6)
        (check-equal? (web-cell-ref (get-field g-cell obj)) 7)
        (check-equal? (web-cell-ref (get-field h-cell obj)) 8)))
    
    (test-case "init-cell and init-field: initial values are correct"
      (let ([obj (singleton/cells component% ()
                   (init-field [a 1] #:accessor)
                   (init-field b 2 #:accessor)
                   (init-cell [c 3] #:accessor)
                   (init-cell d 4 #:accessor)
                   (init-field [e 5])
                   (init-field f 6)
                   (init-cell [g 7])
                   (init-cell h 8))])
        (check-equal? (send obj get-a) 1)
        (check-equal? (send obj get-b) 2)
        (check-equal? (send obj get-c) 3)
        (check-equal? (send obj get-d) 4)
        (check-equal? (get-field e obj) 5)
        (check-equal? (get-field f obj) 6)
        (check-equal? (web-cell-ref (get-field g-cell obj)) 7)
        (check-equal? (web-cell-ref (get-field h-cell obj)) 8)))
    
    (test-case "cells and fields: initial values are correct"
      (let ([obj (singleton/cells component% ()
                   (fields ([(a 1) #:accessor]
                            [b 2 #:accessor]
                            [(e 5)]
                            [f 6]))
                   (cells ([(c 3) #:accessor]
                           [d 4 #:accessor]
                           [(g 7)]
                           [h 8])))])
        (check-equal? (send obj get-a) 1)
        (check-equal? (send obj get-b) 2)
        (check-equal? (send obj get-c) 3)
        (check-equal? (send obj get-d) 4)
        (check-equal? (get-field e obj) 5)
        (check-equal? (get-field f obj) 6)
        (check-equal? (web-cell-ref (get-field g-cell obj)) 7)
        (check-equal? (web-cell-ref (get-field h-cell obj)) 8)))
    
    (test-case "init-cells and init-fields: initial values are correct"
      (let ([obj (singleton/cells component% ()
                   (init-fields ([(a 1) #:accessor]
                                 [b 2 #:accessor]
                                 [(e 5)]
                                 [f 6]))
                   (init-cells ([(c 3) #:accessor]
                                [d 4 #:accessor]
                                [(g 7)]
                                [h 8])))])
        (check-equal? (send obj get-a) 1)
        (check-equal? (send obj get-b) 2)
        (check-equal? (send obj get-c) 3)
        (check-equal? (send obj get-d) 4)
        (check-equal? (get-field e obj) 5)
        (check-equal? (get-field f obj) 6)
        (check-equal? (web-cell-ref (get-field g-cell obj)) 7)
        (check-equal? (web-cell-ref (get-field h-cell obj)) 8)))))

; Provide statements -----------------------------

(provide syntax-tests)
