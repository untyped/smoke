#lang scheme/base

(require (for-syntax scheme/base
                     (only-in srfi/13 string-filter)
                     (planet untyped/unlib:3/debug)
                     (planet untyped/unlib:3/syntax)
                     "syntax-internal.ss")
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
     (let ([seed (foldl expand-clause (make-seed) (syntax->list #'(clause ...)))])
       (quasisyntax/loc stx
         (class* superclass (interface ...)
           #,@(reverse (seed-body-clause-stxs seed))
           #,@(if (seed-has-super-new? seed)
                  null
                  (list #'(super-new)))
           #,@(reverse (seed-foot-clause-stxs seed))
           #,@(if (seed-has-inspector? seed)
                  null
                  (list #'(inspect #f))))))]))

(define-syntax (mixin/cells stx)
  (syntax-case stx ()
    [(_ (interface-in ...) (interface-out ...) clause ...)
     (let ([seed (foldl expand-clause (make-seed) (syntax->list #'(clause ...)))])
       (quasisyntax/loc stx
         (mixin (interface-in ...) (interface-out ...)
           #,@(reverse (seed-body-clause-stxs seed))
           #,@(if (seed-has-super-new? seed)
                  null
                  (list #'(super-new)))
           #,@(reverse (seed-foot-clause-stxs seed))
           #,@(if (seed-has-inspector? seed)
                  null
                  (list #'(inspect #f))))))]))

(define-syntax (singleton/cells stx)
  (syntax-case stx ()
    [(_ superclass (interface ...) clause ...)
     (with-syntax ([prefix (filter-id (or (and (syntax-local-name)
                                               (symbol->string (syntax-local-name)))
                                          "smoke"))])
       #'(parameterize ([inferred-id-prefix 'prefix])
           (new (class/cells superclass (interface ...) clause ...))))]))

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

; Provide statements -----------------------------

(provide (except-out (all-from-out scheme/class)
                     new)
         (rename-out [new/inferred-id new])
         class/cells
         mixin/cells
         singleton/cells
         inferred-id-prefix)
