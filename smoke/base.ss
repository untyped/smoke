#lang scheme/base
  
(require scheme/class
         scheme/contract
         scheme/list
         scheme/match
         scheme/pretty
         scheme/runtime-path
         srfi/26
         (planet untyped/mirrors:1/mirrors)
         (planet untyped/unlib:3/debug)
         (planet untyped/unlib:3/exn)
         (planet untyped/unlib:3/log)
         "web-server/servlet.ss")

; Configuration --------------------------------

; path
(define-runtime-path smoke-htdocs-path 
  "htdocs")

; path
(define-runtime-path smoke-mime-types-path
  "mime.types")

; (parameter (U 'production 'development 'test))
(define current-deployment-mode
  (make-parameter 'production))

; -> boolean
(define (deploying-for-development?)
  (eq? (current-deployment-mode) 'development))

; -> boolean
(define (deploying-for-production?)
  (eq? (current-deployment-mode) 'production))

; -> boolean
(define (deploying-for-test?)
  (eq? (current-deployment-mode) 'test))

; Exceptions -----------------------------------

; (struct string continuation-marks)
(define-struct (exn:fail:smoke exn:fail) () #:transparent)

; (struct string continuation-marks)
(define-struct (exn:fail:smoke:session exn:fail:smoke) () #:transparent)

; (struct string continuation-marks)
(define-struct (exn:smoke exn) () #:transparent)

; (struct string continuation-marks form-element<%>)
(define-struct (exn:smoke:form exn:smoke) (element) #:transparent)

; Provide statements --------------------------- 

(provide (all-from-out scheme/class
                       scheme/contract
                       scheme/list
                       scheme/match
                       scheme/pretty
                       srfi/26
                       (planet untyped/mirrors:1/mirrors)
                       (planet untyped/unlib:3/debug)
                       (planet untyped/unlib:3/exn)
                       (planet untyped/unlib:3/log)
                       "web-server/servlet.ss"))

(provide/contract
 [smoke-htdocs-path                              path?]
 [smoke-mime-types-path                          path?]
 [current-deployment-mode                        (parameter/c (symbols 'development 'production 'test))]
 [deploying-for-development?                     (-> boolean?)]
 [deploying-for-production?                      (-> boolean?)]
 [deploying-for-test?                            (-> boolean?)]
 [struct (exn:fail:smoke exn:fail)               ([message string?] [continuation-marks continuation-mark-set?])]
 [struct (exn:fail:smoke:session exn:fail:smoke) ([message string?] [continuation-marks continuation-mark-set?])]
 [struct (exn:smoke exn)                         ([message string?] [continuation-marks continuation-mark-set?])]
 [struct (exn:smoke:form exn:smoke)              ([message string?] [continuation-marks continuation-mark-set?] [element object?])])
