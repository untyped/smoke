#lang scheme/base

(require (for-syntax scheme/base
                     (only-in srfi/13 string-filter)
                     (planet untyped/unlib:3/debug)
                     (planet untyped/unlib:3/syntax)
                     "syntax-internal.ss")
         scheme/class)

(define inferred-id-prefix
  (make-parameter 'smoke))

; Syntax -----------------------------------------

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
     (with-syntax ([prefix (or (syntax-local-name) 'smoke)])
       #'(parameterize ([inferred-id-prefix 'prefix])
           (new (class/cells superclass (interface ...) clause ...))))]))

(define-for-syntax (class->id-prefix stx)
  ; string -> string
  (define (filter-id str)
    (string-filter (lambda (chr)
                     (or (char-alphabetic? chr)
                         (char-numeric? chr)
                         (memq chr '(#\- #\_))))
                   str))
  (syntax-case stx ()
    [x (identifier? #'x)
       (filter-id (symbol->string (syntax->datum #'x)))]
    [_ #f]))
  
(define-syntax (new/inferred-id stx)
  (syntax-case stx ()
    [(_ class arg ...)
     (with-syntax ([prefix (or (syntax-local-name)
                               (class->id-prefix #'class)
                               'smoke)])
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
