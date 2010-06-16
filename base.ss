#lang scheme/base

(require scheme/contract
         scheme/list
         scheme/match
         scheme/pretty
         scheme/runtime-path
         srfi/26
         (planet untyped/unlib:3/debug)
         (planet untyped/mirrors:2)
         (planet untyped/unlib:3/exn)
         (planet untyped/unlib:3/log)
         "class/class.ss"
         "web-server/servlet.ss")

; Configuration --------------------------------

; (parameter boolean)
(define dev? (make-parameter #f))

; path
(define-runtime-path smoke-htdocs-path 
  "htdocs")

; path
(define-runtime-path smoke-mime-types-path
  "mime.types")

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

(provide (all-from-out scheme/contract
                       scheme/list
                       scheme/match
                       scheme/pretty
                       srfi/26
                       (planet untyped/mirrors:2)
                       (planet untyped/unlib:3/debug)
                       (planet untyped/unlib:3/exn)
                       (planet untyped/unlib:3/log)
                       "class/class.ss"
                       "web-server/servlet.ss"))

(provide/contract
 [dev?                                           (parameter/c boolean?)]
 [smoke-htdocs-path                              path?]
 [smoke-mime-types-path                          path?]
 [struct (exn:fail:smoke exn:fail)               ([message string?] [continuation-marks continuation-mark-set?])]
 [struct (exn:fail:smoke:session exn:fail:smoke) ([message string?] [continuation-marks continuation-mark-set?])]
 [struct (exn:smoke exn)                         ([message string?] [continuation-marks continuation-mark-set?])]
 [struct (exn:smoke:form exn:smoke)              ([message string?] [continuation-marks continuation-mark-set?] [element object?])])
