#lang scheme/base

(require (planet untyped/unlib:3/require))

(require scheme/cmdline
         "../web-server/dispatch3-wrapper.ss"
         "content-base.ss"
         (directory-in "content"))

; -> void
(define (run-application)
  (serve/smoke (lambda ()
                 (testapp-dispatch (current-request)))
               #:htdocs-paths (list testapp-htdocs-path)))

; Main program body ------------------------------

; symbol
(define mode 'runapp)

; void
(command-line #:once-any 
              [("--test")   "Run in test mode."        (set! mode 'test)]
              [("--initdb") "Initialise the database." (set! mode 'initdb)])

; void
(case mode
  [(runapp) (run-application)])
