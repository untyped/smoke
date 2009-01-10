#lang scheme/base

(require (planet untyped/unlib:3/hash)
         "../test-base.ss"
         "syntax.ss")

; Tests ------------------------------------------

(define syntax-tests
  (test-suite "syntax.ss"
    
    (test-case "class name inferred correctly"
      (check-equal? (format "~a" (let ([test% (class/cells object% ())]) test%)) "#<class:test%>" "let")
      (check-not-false (regexp-match #rx"#<class:.*>" (format "~a" (class/cells object% ()))) "anonymous"))))

; Provide statements -----------------------------

(provide syntax-tests)
