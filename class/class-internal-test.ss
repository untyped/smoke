#lang scheme/base

(require "../test-base.ss"
         "class.ss")

; Test data --------------------------------------

(define mix    (mixin/cells () ()))
(define super% (class/cells object% ()))
(define mix%   (mix super%))
(define mid%   (class/cells mix% ()))
(define sub%   (class/cells mid% ()))
(define obj    (new sub%))

; Tests ------------------------------------------

(define/provide-test-suite class-internal-tests
  
  (test-eq? "object-class"
    (object-class obj)
    sub%)
  
  (test-equal? "object-classes"
    (object-classes obj)
    (list super% mix% mid% sub%))
  
  (test-case "object-field/name"
    (let ([obj (singleton/cells object% ()
                 (field [dave 123]))])
      (check-eq? (object-field/name obj 'dave) 123)
      (check-exn exn:fail? (cut object-field/name obj 'noel))))
  
  (test-case "class-name"
    (check-eq? (class-name sub%)   'sub%)
    (check-eq? (class-name mid%)   'mid%)
    (check-eq? (class-name super%) 'super%))
  
  (test-case "class-superclass"
    (check-eq? (class-superclass sub%)   mid%)
    (check-eq? (class-superclass mid%)   mix%)
    (check-eq? (class-superclass mix%)   super%)
    (check-eq? (class-superclass super%) #f))
  
  (test-case "class-ancestors"
    (check-equal? (class-ancestors sub%)   (list super% mix% mid% sub%))
    (check-equal? (class-ancestors mid%)   (list super% mix% mid%))
    (check-equal? (class-ancestors mix%)   (list super% mix%))
    (check-equal? (class-ancestors super%) (list super%))))
