#lang scheme

(require (planet untyped/unlib:3/hash)
         "../test-base.ss"
         "../lib/component.ss"
         "../lib/html/html-element.ss"
         "syntax.ss")

; Tests ------------------------------------------

(define/provide-test-suite syntax-tests
  
  (test-case "class name inferred correctly"
    (check-equal? (format "~a" (let ([test% (class/cells object/cells% ())]) test%)) "#<class:test%>" "let")
    (check-not-false (regexp-match #rx"#<class:.*>" (format "~a" (class/cells object/cells% ()))) "anonymous"))
  
  (test-case "IDs inferred correctly"
    (let ([test/element% (class/cells html-element% ())])
      (check-not-false (regexp-match #rx"^html-element" (symbol->string (send (new html-element%) get-id))))
      (check-not-false (regexp-match #rx"^test_element" (symbol->string (send (new test/element%) get-id))))
      (check-not-false (regexp-match #rx"^foo$" (symbol->string (send (new test/element% [id 'foo]) get-id))))))
  
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
      (check-equal? (web-cell-ref (get-field h-cell obj)) 8)))
  
  (test-case "#:child, #:children, #:optional-child, #:child-transform"
    (match-let* ([(list child0 child1 child2 child3 child4 child5 child6 child7 child8 child9)
                  (for/list ([i (in-range 10)])
                    (new text-field% [id (string->symbol (format "child~a" i))]))]
                 [page (singleton/cells html-component% ()
                         (cell cell0 child0)
                         (cell cell1 child1 #:child)
                         (cell cell2 (list child2 child3) #:children)
                         (cell cell3 #f #:optional-child)
                         (cell cell4 child4 #:optional-child)
                         (cell cell5 #f #:child-transform (lambda (x) (or x null)))
                         (cell cell6 (list child5 child6)
                           #:child-transform (lambda (x) (or x null)))
                         (cell cell7 (list (cons child7 1) (cons child8 2))
                           #:child-transform (lambda (x) (map car x)))
                         (super-new))])
      (check-equal? (sort (send page get-child-components)
                          (lambda (a b)
                            (string<? (symbol->string (send a get-id))
                                      (symbol->string (send b get-id)))))
                    (list child1 child2 child3 child4 child5 child6 child7 child8))))
  
  (test-suite "#:callback, #:callback*"
    
    #:before
    (lambda ()
      (current-page-set!
       (singleton/cells html-page% ()
         (define/override (respond) 'respond))))
    
    (test-case "class/cells"
      (let ([a%   (class/cells object/cells% ()
                    (define/public #:callback (method1) 'a1)
                    (define/public (method2) 'a2))]
            [b%   (class/cells object/cells% ()
                    (define/public (method1) 'b1)
                    (define/public #:callback* (method2) 'b2))])
        ; Correct callback registered:
        (check-true  (send (new a%) callback-registered? 'method1))
        (check-false (send (new a%) callback-registered? 'method2))
        (check-false (send (new b%) callback-registered? 'method1))
        (check-true  (send (new b%) callback-registered? 'method2))
        ; Correct return values:
        (check-equal? (send/apply (new a%) invoke-callback 'method1 null) 'respond)
        (check-exn exn:fail? (cut send/apply (new a%) invoke-callback 'method2 null))
        (check-exn exn:fail? (cut send/apply (new b%) invoke-callback 'method1 null))
        (check-equal? (send/apply (new b%) invoke-callback 'method2 null) 'b2)))
    
    (test-case "class/cells : inheritance"
      (let* ([a%   (class/cells object/cells% ()
                     (define/public #:callback (method1) 'a1)
                     (define/public (method2) 'a2))]
             [b%   (class/cells a% ()
                     (define/override (method1) 'b1)
                     (define/override #:callback* (method2) 'b2))])
        ; Correct callback registered:
        (check-true  (send (new a%) callback-registered? 'method1))
        (check-false (send (new a%) callback-registered? 'method2))
        (check-true  (send (new b%) callback-registered? 'method1))
        (check-true  (send (new b%) callback-registered? 'method2))
        ; Correct return values:
        (check-equal? (send/apply (new a%) invoke-callback 'method1 null) 'respond)
        (check-exn exn:fail? (cut send/apply (new a%) invoke-callback 'method2 null))
        (check-equal? (send/apply (new b%) invoke-callback 'method1 null) 'respond)
        (check-equal? (send/apply (new b%) invoke-callback 'method2 null) 'b2)))
    
    (test-case "class/cells : inheritance : placeholder for the following message"
      (fail (string-append "TODO : require the user to specify "
                           "the same #:callback keyword "
                           "when overriding a method")))
    
    (test-case "mixin/cells"
      (let* ([a%   (class/cells object/cells% ()
                     (define/public #:callback  (method1) 'a1)
                     (define/public #:callback* (method2) 'a2))]
             [mix  (mixin/cells () ()
                     (define/public #:callback  (method3) 'm3)
                     (define/public #:callback* (method4) 'm4))]
             [b%   (mix a%)])
        ; Correct callback registered:
        (check-true  (send (new a%) callback-registered? 'method1))
        (check-true  (send (new a%) callback-registered? 'method2))
        (check-false (send (new a%) callback-registered? 'method3))
        (check-false (send (new a%) callback-registered? 'method4))
        (check-true  (send (new b%) callback-registered? 'method1))
        (check-true  (send (new b%) callback-registered? 'method2))
        (check-true  (send (new b%) callback-registered? 'method3))
        (check-true  (send (new b%) callback-registered? 'method4))
        ; Correct return values:
        (check-equal? (send/apply (new a%) invoke-callback 'method1 null) 'respond)
        (check-equal? (send/apply (new a%) invoke-callback 'method2 null) 'a2)
        (check-equal? (send/apply (new b%) invoke-callback 'method1 null) 'respond)
        (check-equal? (send/apply (new b%) invoke-callback 'method2 null) 'a2)
        (check-equal? (send/apply (new b%) invoke-callback 'method3 null) 'respond)
        (check-equal? (send/apply (new b%) invoke-callback 'method4 null) 'm4)))
    
    (test-case "singleton/cells"
      (let* ([a%   (class/cells object/cells% ()
                     (define/public #:callback (method1) 'a1)
                     (define/public (method2) 'a2))]
             [b    (singleton/cells a% ()
                     (define/override (method1) 'b1)
                     (define/override #:callback* (method2) 'b2))])
        ; Correct callback registered:
        (check-true  (send b callback-registered? 'method1))
        (check-true  (send b callback-registered? 'method2))
        ; Correct return values:
        (check-equal? (send/apply b invoke-callback 'method1 null) 'respond)
        (check-equal? (send/apply b invoke-callback 'method2 null) 'b2)))))