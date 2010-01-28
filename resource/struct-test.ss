#lang scheme/base

(require srfi/26
         "../test-base.ss"
         "struct.ss"
         (prefix-in internal: "struct-internal.ss"))

; Test data --------------------------------------

(define-js-resource a "a.js" ())
(define-js-resource b "b.js" ())
(define-compound-resource c (a b))
(define-css-resource d "a.css" ())
(define-css-resource e "b.css" ())
(define-compound-resource f (d e))
(define-compound-resource g (c f))

; Tests ------------------------------------------

(define struct-tests
  (test-suite "struct.ss"
    
    (test-case "web-resource-dependency?"
      (check-true (web-resource-dependency? c a))
      (check-true (web-resource-dependency? c b))
      (check-true (web-resource-dependency? g a))
      (check-false (web-resource-dependency? d a))
      (check-false (web-resource-dependency? a c))
      (check-false (web-resource-dependency? a g))
      (check-false (web-resource-dependency? a d))
      (check-false (web-resource-dependency? a a)))
    
    (test-case "web-resource<?"
      (check-true (internal:web-resource<? a b))
      (check-false (internal:web-resource<? a c))
      (check-true (internal:web-resource<? a f))
      (check-true (internal:web-resource<? a d))
      (check-false (internal:web-resource<? d b))
      (check-true (internal:web-resource<? d c))
      (check-true (internal:web-resource<? d e))
      (check-true (internal:web-resource<? c b))
      (check-false (internal:web-resource<? c d))
      (check-true (internal:web-resource<? c f)))
  
    (test-case "web-resource-plan caches new plans"
      (map (lambda (res)
             (with-check-info (['resource res])
               (check-false (internal:web-resource-plan res))
               (check-equal? (web-resource-plan res) (list res))
               (check-equal? (internal:web-resource-plan res) (list res))))
           (list (make-js-resource "foo.js")
                 (make-css-resource "foo.css")
                 (make-compound-resource "foo" null))))
    
    (test-case "plan contains correct resources"
      (let ([plan (web-resource-plan c)])
        (for ([res (in-list (list a b c))])
          (check-not-false (memq res plan)))
        (for ([res (in-list (list d e f g))])
          (check-false (memq res plan)))))
    
    (test-case "plan satisfies dependency constraints"
      (let ([plan (web-resource-plan g)])
        (for ([res1 (in-list (list a b c d e f g))])
          (for ([res2 (in-list (internal:web-resource-prev res1))])
            (check-not-false (memq res2 (memq res1 plan)))))))
    
    (test-case "dependencies added and removed correctly"
      ; Test precondition:
      (check-equal? (internal:web-resource-prev a) null)
      (check-equal? (internal:web-resource-next a) (list c))
      (check-equal? (internal:web-resource-prev b) null)
      (check-equal? (internal:web-resource-next b) (list c))
      ; Add the dependency b->a:
      (add-web-resource-dependency! a b)
      (check-equal? (internal:web-resource-prev a) (list b))
      (check-equal? (internal:web-resource-next a) (list c))
      (check-equal? (internal:web-resource-prev b) null)
      (check-not-false (memq a (internal:web-resource-next b)))
      (check-not-false (memq c (internal:web-resource-next b)))
      ; Remove the dependency b->a:
      (remove-web-resource-dependency! a b)
      (check-equal? (internal:web-resource-prev a) null)
      (check-equal? (internal:web-resource-next a) (list c))
      (check-equal? (internal:web-resource-prev b) null)
      (check-equal? (internal:web-resource-next b) (list c)))
    
    (test-case "cyclic depdendencies are guarded against"
      (check-exn exn:fail? (cut add-web-resource-dependency! a c))
      (check-exn exn:fail? (cut add-web-resource-dependency! a g)))
    
    (test-case "changing the dependency graph invalidates dependent plans"
      ; Retrieve the plan from g: all resources should have cached plans:
      (web-resource-plan g)
      (check-not-false (internal:web-resource-plan g))
      (check-not-false (internal:web-resource-plan a))
      (check-not-false (internal:web-resource-plan d))
      ; Add a dependency b->a: a, b, c and g should be invalidated:
      (check-not-exn (cut add-web-resource-dependency! a b))
      (check-false (internal:web-resource-plan g))
      (check-false (internal:web-resource-plan a))
      (check-not-false (internal:web-resource-plan d))
      ; Retrieve the plan from c: a, b and c should be recached:
      (web-resource-plan c)
      (check-false (internal:web-resource-plan g))
      (check-not-false (internal:web-resource-plan a))
      (check-not-false (internal:web-resource-plan d))
      ; Postcondition: remove the dependency again:
      (remove-web-resource-dependency! a b))
    
    (test-case "plan reconfigures itself as necessary"
      ; Test precondition: a and b are in c's plan:
      (check-not-false (memq a (memq c (web-resource-plan c))))
      (check-not-false (memq b (memq c (web-resource-plan c))))
      (check-false (memq a (web-resource-plan b)))
      (check-false (memq b (web-resource-plan a)))
      ; Adding a dependency b->a should set the plan order one way:
      (check-not-exn (cut add-web-resource-dependency! a b))
      (check-equal? (web-resource-plan c) (list c a b))
      (remove-web-resource-dependency! a b)
      ; Changing the dependency to a->b should set the plan order another way:
      (check-not-exn (cut add-web-resource-dependency! b a))
      (check-equal? (web-resource-plan c) (list c b a))
      (remove-web-resource-dependency! b a))))

; Provide statements -----------------------------

(provide struct-tests)
