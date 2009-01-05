#lang scheme/base

(require scheme/contract)

; Resume continuations ---------------------------

; prompt
(define resume-prompt
  (make-continuation-prompt-tag))

; (parameter (continuation -> void))
(define resume-parameter
  (make-parameter #f))

; -> boolean
(define (resume-available?)
  (and (resume-parameter) #t))

; -> any
(define (resume-from-here)
  (call-with-composable-continuation
   (lambda (k) 
     ((resume-parameter) k))
   resume-prompt))

; Provide statements -----------------------------

(provide/contract
 [resume-prompt     continuation-prompt-tag?]
 [resume-parameter  (parameter/c (-> continuation? void?))]
 [resume-available? (-> boolean?)]
 [resume-from-here  procedure?])
