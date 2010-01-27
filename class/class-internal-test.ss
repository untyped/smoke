#lang scheme

(require "../test-base.ss"
         "class.ss")

; Tests ------------------------------------------

(define class-internal-tests
  (test-suite "class-internal.ss"
    
    (test-case "object-classes"
      (let* ([mix    (mixin/cells () ())]
             [super% (class/cells object% ())]
             [mixed  (mix super%)]
             [mid%   (class/cells mixed ())]
             [sub%   (class/cells mid% ())]
             [obj    (new sub%)])
        (check-equal? (object-classes obj)
                      (list super% mixed mid% sub%)
                      "object-classes")
        (check-equal? (class-ancestors sub%)
                      (list super% mixed mid% sub%)
                      "class-ancestors")))
    
    ))

; Provide statements -----------------------------

(provide class-internal-tests)
