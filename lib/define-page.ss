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

(define-syntax (define-controller complete-stx)
  
  (define procedure-style? #f)
  (define id-stx           #f)
  (define args-stx         #f)
  (define rest-stx         #f)
  (define wrapper-proc-stx #f)
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
    (with-syntax* ([id               id-stx]
                   [(arg ...)        args-stx]
                   [body             (if procedure-style?
                                         (quasisyntax/loc complete-stx
                                           (lambda (arg ...) #,@body-stx))
                                         (car (syntax->list body-stx)))]
                   [box-id           (page-info-box-id (page-info-ref id-stx))]
                   [page-id          (make-id #f id-stx)]
                   [wrapper-proc     wrapper-proc-stx]
                   [access-proc      access-proc-stx]
                   [denied-proc      denied-proc-stx])
      (quasisyntax/loc complete-stx
        (begin
          (define-object page-id page% ()
            
            #,@(if access-proc-stx
                   #'((define/override access-allowed?
                        access-proc))
                   #'())
            
            #,@(if denied-proc-stx
                   #'((define/override access-denied
                        denied-proc))
                   #'())
            
            #,@(if wrapper-proc-stx
                   #'((define/override (dispatch . args)
                        (apply wrapper-proc
                               (lambda args
                                 (dispatch . args)
                                 args))))
                   #'())
            
            (define/override dispatch
              body))
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

(provide define-page
         define-controller)