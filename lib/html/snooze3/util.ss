#lang scheme/base

(require srfi/13
         (planet untyped/snooze:3/snooze)
         (planet untyped/snooze:3/check/check)
         "../../../lib-base.ss")

; (listof symbol)
(define check-result-classes
  '(check-success check-warning check-failure check-fatal))

; annotation
(define-annotation ann:form-elements
  (lambda (result) null)
  (lambda (result old new) (append old new)))

; Procedures -------------------------------------

; attribute ... -> (check-result -> boolean)
(define (by-attributes . attrs)
  (lambda (result)
    (and (ormap (cut check-result-has-attribute? result <>) attrs) #t)))

; symbol -> (check-result -> boolean)
(define (by-annotation ann val)
  (lambda (result)
    (equal? (check-result-annotation result ann) val)))

; symbol -> boolean
(define (check-result-class? sym)
  (and (memq sym check-result-classes) #t))

; check-result -> check-class
;
; where check-class : (U 'check-success 'check-warning 'check-failure 'check-fatal)
(define (check-result->class result)
  (cond [(check-success? result) 'check-success]
        [(check-warning? result) 'check-warning]
        [(check-failure? result) 'check-failure]
        [(check-fatal? result)   'check-fatal]))

; (listof check-result) -> check-class
(define (check-results->class results)
  (let loop ([results results] [warnings? #f] [failures? #f])
    (if (null? results)
        (cond [failures? 'check-failure]
              [warnings? 'check-warning]
              [else      'check-success])
        (let ([head (car results)]
              [tail (cdr results)])
          (cond [(check-fatal? head)   'check-fatal]
                [(check-failure? head) (loop tail warnings? #t)]
                [(check-warning? head) (loop tail #t failures?)]
                [else                  (loop tail warnings? failures?)])))))

; symbol -> xml
(define (check-result-icon class)
  (case class
    [(check-success) success-icon]
    [(check-warning) warning-icon]
    [(check-failure) failure-icon]
    [(check-fatal)   fatal-icon]))

; (listof check-result) [#:id symbol] -> void
(define (print-check-fatals results [out (current-output-port)] #:id [id #f])
  (fprintf out "Check exns found in results~a:~n"
           (if id (format " (ID \"~a\")" id) ""))
  (with-pretty-indent "  "
    (pretty-print
     (foldr (lambda (result accum)
              (cond [(check-success? result) accum]
                    [(check-warning? result) (cons result accum)]
                    [(check-failure? result) (cons result accum)]
                    [(check-fatal? result)   
                     (cons (cons result (exn-context (check-result-exn result))) accum)]))
            null
            results)
     out)))

; (listof (U symbol string)) -> string
(define (format-classes classes)
  (string-join (map (lambda (class)
                      (if (string? class)
                          class
                          (symbol->string class)))
                    classes)
               " "))

; Helpers ----------------------------------------

; xml
(define success-icon
  (xml (img (@ [src   "/images/smoke/success.png"]
               [class "check-success icon"]
               [alt   "Success"]
               #;[title "Success"]))))

; xml
(define warning-icon
  (xml (img (@ [src   "/images/smoke/warning.png"]
               [class "check-warning icon"]
               [alt   "Warning"]
               #;[title "Warning"]))))

; xml
(define failure-icon
  (xml (img (@ [src   "/images/smoke/failure.png"]
               [class "check-failure icon"]
               [alt   "Failure"]
               #;[title "Failure"]))))

; xml
(define fatal-icon
  (xml (img (@ [src   "/images/smoke/exn.png"]
               [class "check-fatal icon"]
               [alt   "Error"]
               #;[title "Error"]))))

; xml
(define snooze-styles
  (xml (link   (@ [rel  "stylesheet"]
                  [type "text/css"]
                  [href "/style/smoke/snooze.css"]))))


; Provide statements -----------------------------

(provide/contract
 [ann:form-elements    annotation?]
 [by-attributes        (->* () () #:rest (listof attribute?) (-> check-result? boolean?))]
 [by-annotation        (-> annotation? any/c (-> check-result? boolean?))]
 [check-result-classes (listof symbol?)]
 [check-result-class?  (-> symbol? boolean?)]
 [check-result->class  (-> check-result? check-result-class?)]
 [check-results->class (-> (listof check-result?) check-result-class?)]
 [check-result-icon    (-> check-result-class? xml?)]
 [print-check-fatals   (->* ((listof check-result?)) (output-port? #:id symbol?) void?)]
 [format-classes       (-> (listof (or/c symbol? string?)) string?)]
 [success-icon         xml?]
 [warning-icon         xml?]
 [failure-icon         xml?]
 [fatal-icon           xml?]
 [snooze-styles        xml?])
