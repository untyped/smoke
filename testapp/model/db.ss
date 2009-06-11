#lang scheme/base

(require (planet untyped/snooze:3)
         (planet untyped/snooze:3/postgresql8/postgresql8))

(current-snooze (make-snooze (make-postgresql8-database #:database "smoketest" #:username "smoketest")))

(provide (all-from-out (planet untyped/snooze:3)))
