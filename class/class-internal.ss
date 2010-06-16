#lang scheme/base

(require (only-in srfi/1 list-index)
         scheme/class
         scheme/contract
         srfi/26
         "../web-server/web-cell.ss")

; Interfaces -------------------------------------

(define object/cells<%>
  (interface ()
    dirty?))

; Classes ----------------------------------------

(define object/cells%
  (class object%
    
    (inspect #f)
    
    ; Fields -------------------------------------
    
    ; (listof cell)
    ;
    ; This field contains a list of the web-cells that are taken into account by the "dirty?" method.
    (field [web-cell-fields null])
    
    ; Constructor --------------------------------
    
    (super-new)
    
    ; Methods ------------------------------------
    
    ; -> boolean
    ;
    ; Returns #t if this component has changed and/or needs refreshing.
    ;
    ; The default implementation returns #t if any of the component's
    ; web cell content (including child web cells) has changed, and
    ; raises exn:fail if called in the root web frame where changes
    ; cannot have been made.
    (define/public (dirty?)
      (define frame (current-frame))
      (unless (frame-parent frame)
        (error "Cannot call dirty? in the root web frame."))
      (ormap (lambda (cell)
               (and (web-cell-set? cell frame)
                    (web-cell-changed? cell)))
             web-cell-fields))
    
    ; cell -> void
    (define/public (register-web-cell-field! cell)
      (set! web-cell-fields (cons cell web-cell-fields)))))

; Procedures -----------------------------------

; object -> class
(define (object-class obj)
  (define-values (class skipped?)
    (object-info obj))
  (if skipped?
      (error (format "Could not retrieve object metadata for ~a" obj))
      class))

; class -> symbol
(define (class-name class)
  (define-values (name field-k field-names field-accessor field-mutator super-class skipped?)
    (class-info class))
  (if skipped?
      (error (format "Could not retrieve class metadata for ~a" class))
      name))

; object symbol -> any
; Thanks to Danny Yoo for this one.
(define (object-field/name obj name)
  (define-values (class skipped)
    (object-info obj))
  (let loop ([class class])
    (if class
        (let-values ([(name-symbol field-k field-name-list field-accessor-proc field-mutator-proc super-class skipped)
                      (class-info class)])
          (let ([index (list-index (cut eq? <> name) field-name-list)])
            (cond [(not index) (error (format "Field not found: ~a in ~a" name obj))]
                  [(>= index field-k) (loop super-class)]
                  [else (field-accessor-proc obj index)])))
        (error (format "Can't inspect class ~a for ~a" class obj)))))

; object -> (listof class)
(define (object-classes obj)
  (class-ancestors (object-class obj)))

; class -> (listof class)
(define (class-ancestors class)
  (let loop ([class class] [accum (list class)])
    (if class
        (let-values ([(name-symbol field-k field-name-list field-accessor-proc field-mutator-proc super-class skipped)
                      (class-info class)])
          (if super-class
              (loop super-class (cons super-class accum))
              accum))
        (error (format "Can't inspect class ~a" class)))))

; Provide statements -----------------------------

(provide/contract
 [object/cells<%>   interface?]
 [object/cells%     class?]
 [object-class      (-> object? class?)]
 [class-name        (-> class? symbol?)]
 [object-field/name (-> object? symbol? any)]
 [object-classes    (-> object? (listof class?))]
 [class-ancestors   (-> class? (listof class?))])
