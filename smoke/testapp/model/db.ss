#lang scheme/base

(require (planet untyped/snooze:2/snooze)
         (planet untyped/snooze:2/postgresql8/postgresql8))

; snooze%
(define snooze (make-snooze% (make-database% #:database "smoketest" #:username "smoketest")))

(define-snooze-interface snooze)

; Provide statements -----------------------------

(provide (all-from-out (planet untyped/snooze:2/snooze))
         (snooze-interface-out))
