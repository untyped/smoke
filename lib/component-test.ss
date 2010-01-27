#lang web-server

(require (only-in srfi/13/string string-trim-both)
         "../lib-base.ss"
         "../test-base.ss"
         "component.ss")

; Tests ------------------------------------------

(define component-tests
  (test-suite "component.ss"

    (test-exn "dirty? : root frame"
      exn:fail?
      ; The current frame may or may not be the root,
      ; depending on which tests have run before this one:
      (cut call-with-frame (frame-root (current-frame))
           (send n1 dirty?)))

    (test-case "dirty? : no changes"
      (call-with-frame (push-frame)
        (lambda ()
          (for-each (lambda (node expected msg)
                      (check-equal? (send node dirty?) expected msg))
                    (list n1 n2 n3 n4 n5 n6 n7)
                    (list #f #f #f #f #f #f #f)
                    (list 'n1 'n2 'n3 'n4 'n5 'n6 'n7)))))
    
    (test-case "dirty? : node moved"
      (call-with-frame (push-frame)
        (lambda ()
          (send n2 set-children! (list n3))
          (send n7 set-children! (list n4))
          (for-each (lambda (node expected msg)
                      (check-equal? (send node dirty?) expected msg))
                    (list n1 n2 n3 n4 n5 n6 n7)
                    (list #f #t #f #f #f #f #t)
                    (list 'n1 'n2 'n3 'n4 'n5 'n6 'n7))))
      (call-with-frame (push-frame)
        (lambda ()
          (for-each (lambda (node expected msg)
                      (check-equal? (send node dirty?) expected msg))
                    (list n1 n2 n3 n4 n5 n6 n7)
                    (list #f #f #f #f #f #f #f)
                    (list 'n1 'n2 'n3 'n4 'n5 'n6 'n7)))))))

; Helpers ----------------------------------------

(define node%
  (class/cells component% ()
    
    ; -> symbol
    (inherit get-component-id)
    
    ; Fields -------------------------------------
    
    ; (cell boolean)
    (init-cell always-dirty? #f #:accessor #:mutator)
    
    ; (cell (listof component<%>))
    (init-cell children null #:children #:accessor #:mutator)))

(define n7 (new node% [component-id 'n7]))
(define n6 (new node% [component-id 'n6]))
(define n5 (new node% [component-id 'n5] [children (list n6 n7)]))
(define n4 (new node% [component-id 'n4]))
(define n3 (new node% [component-id 'n3]))
(define n2 (new node% [component-id 'n2] [children (list n3 n4)]))
(define n1 (new node% [component-id 'n1] [children (list n2 n5)]))

; Provide statements -----------------------------

(provide component-tests)
