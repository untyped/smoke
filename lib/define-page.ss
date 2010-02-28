#lang scheme

(require "../lib-base.ss")

(require (for-syntax syntax/parse
                     (planet cce/scheme:6/syntax)
                     (planet untyped/unlib:3/debug)
                     (planet untyped/unlib:3/syntax)
                     "site-info.ss")
         "page.ss"
         "site.ss")

(define-syntax (define-page complete-stx)
  (syntax-parse complete-stx
    [(_ (~describe "id" page-id:id)
        (~describe "superclass-id" superclass-id)
        (~describe "interfaces" (interface ...))
        clause ...)
     (with-syntax ([class-id (make-id #f #'page-id '%)])
       (syntax/loc complete-stx
         (begin
           (define-class class-id superclass-id (interface ...) clause ...)
           (define-page page-id (new class-id)))))]
    [(_ (~describe "page id" page-id:id)
        (~describe "expr" expr))
     (with-syntax ([box-id   (or (lookup-page-box #'page-id)
                                 (raise-syntax-error #f "page must also be defined in a define-site expression"
                                                     complete-stx
                                                     #'page-id))]
                   [new-page (syntax->datum #'page-id)])
       (syntax/loc complete-stx
         (define _
           (let ([old-page (unbox box-id)]
                 [new-page expr])
             (if (is-a? new-page page<%>)
                 (set-box! box-id new-page)
                 (raise-type-error 'define-page "page<%>" new-page))
             (send new-page set-site! (send old-page get-site))
             (send old-page set-site! #f)
             new-page))))]))

; Legacy controller compatibility ----------------

; Not provided by module:
(define-struct (exn:fail:no-access exn:fail) ())
    
(define (plain-controller-wrapper page . args)
  (with-handlers ([exn:fail:no-access? (lambda _ (send/apply page access-denied args))])
    (dynamic-wind
     (lambda ()
       (unless (send/apply page access-allowed? args)
         (raise-exn exn:fail:no-access "Access denied.")))
     (lambda ()
       (send/apply page dispatch args))
     void)))

(define default-controller-wrapper
  (make-parameter plain-controller-wrapper))

(define-syntax (define-controller complete-stx)
  
  (define procedure-style? #f)
  (define id-stx           #f)
  (define args-stx         #f)
  (define rest-stx         #f)
  (define wrapper-proc-stx #'(default-controller-wrapper))
  (define access-proc-stx  #f)
  (define denied-proc-stx  #f)
  
  (define (parse-keywords stx)
    (syntax-case stx ()
      [(#:wrapper-proc proc other ...)
       (begin (set! wrapper-proc-stx #'proc)
              (parse-keywords #'(other ...)))]
      [(#:access? expr other ...)
       (if procedure-style?
           (begin (set! access-proc-stx
                        (with-syntax ([(arg ...) args-stx])
                          #'(lambda (arg ...) expr)))
                  (parse-keywords #'(other ...)))
           (raise-syntax-error #f "#:access? keyword only allowed in procedure-style controller definitions"
                               complete-stx #'(#:access? expr)))]
      [(#:access-proc proc other ...)
       (begin (set! access-proc-stx #'proc)
              (parse-keywords #'(other ...)))]
      [(#:denied-proc proc other ...)
       (begin (set! denied-proc-stx #'proc)
              (parse-keywords #'(other ...)))]
      [(kw other ...)
       (keyword? (syntax->datum #'kw))
       (raise-syntax-error #f "unrecognised define-controller keyword"
                           complete-stx #'kw)]
      [(rest) (parse-body #'(rest))]
      [()     (raise-syntax-error #f "no controller body specified" complete-stx)]
      [rest   (if procedure-style?
                  (parse-body #'rest)
                  (raise-syntax-error #f "too many body expressions for non-procedure-style controller definition"
                                      complete-stx #'rest))]))
  
  (define (parse-body body-stx)
    (with-syntax* ([id           id-stx]
                   [(arg ...)    args-stx]
                   [body         (if procedure-style?
                                     (quasisyntax/loc complete-stx
                                       (lambda (arg ...) #,@body-stx))
                                     (car (syntax->list body-stx)))]
                   [box-id       (page-info-box-id (page-info-ref id-stx))]
                   [page-id      (make-id #f id-stx)]
                   [wrapper-proc wrapper-proc-stx]
                   [access-proc  access-proc-stx]
                   [denied-proc  denied-proc-stx])
      (quasisyntax/loc complete-stx
        (begin
          (define-object page-id page% ()
            
            #,@(if access-proc-stx
                   #'((define/override (access-allowed? . args)
                        (apply access-proc args)))
                   #'())
            
            #,@(if denied-proc-stx
                   #'((define/override (access-denied . args)
                        (apply denied-proc args)))
                   #'())
            
            (define/override (dispatch-initial . args)
              (apply wrapper-proc this args))
            
            (define/override (dispatch . args)
              (apply body args)))
          (define _
            (let ([old-page (unbox box-id)])
              (set-box! box-id page-id)
              (send page-id set-site! (send old-page get-site))
              (send old-page set-site! #f)))))))
  
  (syntax-parse complete-stx
    [(_ (id:id arg ...) keyword+expr ...)
     (begin (set! procedure-style? #t)
            (set! id-stx           #'id)
            (set! args-stx         #'(arg ...))
            (parse-keywords        #'(keyword+expr ...)))]
    [(_ id:id keyword+expr ...)
     (begin (set! procedure-style? #f)
            (set! id-stx           #'id)
            (set! args-stx         null)
            (parse-keywords        #'(keyword+expr ...)))]))

; Provides ---------------------------------------

(define controller-wrapper/c
  (->* ((is-a?/c page<%>)) () #:rest any/c any))

(provide define-page
         define-controller)

(provide/contract
 [plain-controller-wrapper   controller-wrapper/c]
 [default-controller-wrapper (parameter/c controller-wrapper/c)])
