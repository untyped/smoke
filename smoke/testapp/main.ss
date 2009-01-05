#lang scheme/base

(require (planet untyped/unlib:3/require))

(require scheme/cmdline
         "content-base.ss"
         (directory-in "content"))

; -> void
(define (run-application)
  (current-deployment-mode 'development)
  (run-smoke #:start       (lambda ()
                             (parameterize ([dispatch-url-cleaner smoke-url-cleaner])
                               (dispatch (current-request) test-site)))
             #:htdocs-path (list testapp-htdocs-path)))

; Main program body ------------------------------

; symbol
(define mode 'runapp)

; void
(command-line #:once-any 
              [("--test") "Run in test mode." (set! mode 'test)]
              [("--initdb") "Initialise the database." (set! mode 'initdb)])

; void
(case mode
  [(runapp) (run-application)])
