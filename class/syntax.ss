#lang scheme/base

(require (for-syntax scheme/base
                     (only-in srfi/13 string-filter)
                     (planet untyped/unlib:3/debug)
                     (planet untyped/unlib:3/syntax)
                     "syntax-internal.ss")
         (for-template "callback-registry.ss")
         scheme/class)

(define inferred-id-prefix
  (make-parameter "smoke"))

; Syntax -----------------------------------------

; string -> string
(define-for-syntax (filter-id str)
  (regexp-replace* #rx"[^a-zA-Z0-9_-]" str "_"))

(define-syntax (class/cells stx)
  (syntax-case stx ()
    [(_ superclass (interface ...) clause ...)
     (with-syntax ([class-id (or (syntax-local-name) 'class%)])
       (let ([seed (foldl expand-clause (make-seed #'class-id) (syntax->list #'(clause ...)))])
         (quasisyntax/loc stx
           (let ([class-id (class* superclass (interface ...)
                             #,@(reverse (seed-body-clause-stxs seed))
                             #,@(if (seed-has-super-new? seed)
                                    null
                                    (list #'(super-new)))
                             #,@(reverse (seed-foot-clause-stxs seed))
                             #,@(if (seed-has-inspector? seed)
                                    null
                                    (list #'(inspect #f))))])
             #,@(reverse (seed-post-clause-stxs seed))
             class-id))))]))

(define-syntax (mixin/cells stx)
  (syntax-case stx ()
    [(_ (interface-in ...) (interface-out ...) clause ...)
     (with-syntax ([class1 (if (syntax-local-name)
                               (make-id #f (syntax-local-name) '-mixed%)
                               'mixed%)])
       (let ([seed (foldl expand-clause (make-seed #'class1) (syntax->list #'(clause ...)))])
         (quasisyntax/loc stx
           (lambda (class0)
             (let ([class1 ((mixin (interface-in ...) (interface-out ...)
                              #,@(reverse (seed-body-clause-stxs seed))
                              #,@(if (seed-has-super-new? seed)
                                     null
                                     (list #'(super-new)))
                              #,@(reverse (seed-foot-clause-stxs seed))
                              #,@(if (seed-has-inspector? seed)
                                     null
                                     (list #'(inspect #f))))
                            class0)])
               #,@(reverse (seed-post-clause-stxs seed))
               class1)))))]))

(define-syntax (new/inferred-id stx)
  (syntax-case stx ()
    [(_ class arg ...)
     (with-syntax ([prefix (filter-id (or (and (syntax-local-name)
                                               (symbol->string (syntax-local-name)))
                                          (and (identifier? #'class)
                                               (symbol->string (syntax->datum #'class)))
                                          "smoke"))])
       #'(parameterize ([inferred-id-prefix 'prefix])
           (new class arg ...)))]))

(define-syntax (singleton/cells stx)
  (syntax-case stx ()
    [(_ superclass (interface ...) clause ...)
     (with-syntax ([prefix (filter-id (or (and (syntax-local-name)
                                               (symbol->string (syntax-local-name)))
                                          "smoke"))])
       (if (syntax-local-name)
           (with-syntax ([class-id (make-id stx (syntax-local-name) '%)])
             #'(new/inferred-id (let ([class-id (class/cells superclass (interface ...) clause ...)]) class-id)))
           #'(new/inferred-id (class/cells superclass (interface ...) clause ...))))]))

(define-syntax (define-class stx)
  (syntax-case stx ()
    [(_ id superclass (interface ...) clause ...)
     (let ([seed (foldl expand-clause (make-seed #'id) (syntax->list #'(clause ...)))])
       (quasisyntax/loc stx
         (begin
           (define-serializable-class* id superclass (interface ...)
             #,@(reverse (seed-body-clause-stxs seed))
             #,@(if (seed-has-super-new? seed)
                    null
                    (list #'(super-new)))
             #,@(reverse (seed-foot-clause-stxs seed))
             #,@(if (seed-has-inspector? seed)
                    null
                    (list #'(inspect #f))))
           #,@(reverse (seed-post-clause-stxs seed)))))]))

(define-syntax (define-mixin stx)
  (syntax-case stx ()
    [(_ id (interface-in ...) (interface-out ...) clause ...)
     (quasisyntax/loc stx
       (define id
         (mixin/cells (interface-in ...) (interface-out ...)
           clause ...)))]))

(define-syntax (define-singleton stx)
  (syntax-case stx ()
    [(_ id superclass (interface ...) clause ...)
     (with-syntax ([class-id (make-id #'id #'id '%)])
       (quasisyntax/loc stx
         (begin
           (define-class class-id superclass (interface ...) clause ...)
           (define id (new class)))))]))

; Provide statements -----------------------------

(provide (except-out (all-from-out scheme/class) new)
         (rename-out [new/inferred-id new])
         class/cells
         mixin/cells
         singleton/cells
         inferred-id-prefix
         define-class
         define-mixin
         define-singleton)
