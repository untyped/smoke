#lang scheme

(require scheme/runtime-path
         srfi/26
         (planet untyped/unlib:3/debug)
         (planet untyped/mirrors:2)
         (planet untyped/unlib:3/exn)
         (planet untyped/unlib:3/log))

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

; (struct string continuation-marks string)
(define-struct (exn:fail:smoke:callback exn:fail:smoke) (serial) #:transparent)

; (struct string continuation-marks)
(define-struct (exn:smoke exn) () #:transparent)

; (struct string continuation-marks form-element<%>)
(define-struct (exn:smoke:form exn:smoke) (element) #:transparent)



; Provide statements --------------------------- 

(provide (all-from-out srfi/26
                       (planet untyped/mirrors:2)
                       (planet untyped/unlib:3/debug)
                       (planet untyped/unlib:3/exn)
                       (planet untyped/unlib:3/log)))

(provide/contract
 [dev?                                            (parameter/c boolean?)]
 [smoke-htdocs-path                               path?]
 [smoke-mime-types-path                           path?]
 [struct (exn:fail:smoke exn:fail)                ([message string?] [continuation-marks continuation-mark-set?])]
 [struct (exn:fail:smoke:session exn:fail:smoke)  ([message string?] [continuation-marks continuation-mark-set?])]
 [struct (exn:fail:smoke:callback exn:fail:smoke) ([message string?] [continuation-marks continuation-mark-set?] [serial string?])]
 [struct (exn:smoke exn)                          ([message string?] [continuation-marks continuation-mark-set?])]
 [struct (exn:smoke:form exn:smoke)               ([message string?] [continuation-marks continuation-mark-set?] [element object?])])
