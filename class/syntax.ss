#lang scheme/base

(require (for-syntax scheme/base 
                     (planet untyped/unlib:3/syntax)
                     "syntax-internal.ss")
         scheme/class)

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

(define-syntax singleton/cells
  (syntax-rules ()
    [(_ superclass (interface ...) clause ...)
     (new (class/cells superclass (interface ...) clause ...))]))

; Provide statements -----------------------------

(provide class/cells
         mixin/cells
         singleton/cells)
