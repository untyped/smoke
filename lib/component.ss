#lang scheme

(require (planet untyped/unlib:3/symbol)
         "../lib-base.ss")

; Classses ---------------------------------------

(define-class component% object/cells% (component<%> printable<%>)
  
  (inherit dirty?)
  
  ; Fields -------------------------------------
  
  ; (cell symbol)
  (init-field component-id (gensym/interned (inferred-id-prefix)) #:accessor)
  
  ; (cell (alistof symbol (-> (listof component<%>)))
  (cell child-registry null)
    
  ; Request handling ---------------------------
  
  ; There are two methods involved here:
  ;
  ; handle-request calls on-request and then recurses down the tree.
  ; Override this if you don't want the standard recursion behaviour.
  ; 
  ; on-request does whatever is appropriate for this component.
  ; Override this in most cases (when the recursion behaviour is fine).
  
  ; request -> void
  (define/pubment (on-request request)
    (on-request/fold request))
  
  ; request -> void
  (define/public (on-request/fold request)
    (inner (void) on-request request)
    (for-each (cut send <> on-request request)
              (get-child-components)))
  
  ; Rendering ----------------------------------
  
  ; Returns the visible content for this component and its subtree.
  ; Must be overridden in subclasses of component%.
  ; seed -> content
  (define/public (render seed)
    (error "render: must be overridden in a subclass of component%."))
  
  ; Children -----------------------------------
  
  ; Returns a list of all child components, whether they are attached or not.
  ; -> (listof component<%>)
  (define/public (get-child-components)
    (append-map (match-lambda
                  [(list-rest field-name children)
                   (with-handlers ([exn? (lambda (exn)
                                           (error (format "bad child field: ~a" (exn-message exn))
                                                  field-name))])
                     (let ([children (if (procedure? children)
                                         (children)
                                         children)])
                       (if (component-list? children)
                           children
                           (raise-type-error
                            (string->symbol (format "~a.~a" this field-name))
                            "(listof component<%>)"
                            children))))])
                (with-handlers ([exn? (lambda _ (error "no child registry found" this))])
                  (web-cell-ref child-registry-cell))))
  
  ; symbol -> (U component<%> #f)
  (define/public (find-component id)
    (or (and (eq? id (get-component-id)) this)
        (ormap (cut send <> find-component id)
               (get-child-components))))
  
  ; Returns a list of all components in this subtree.
  ; -> (listof component<%>)
  (define/public (get-all-components)
    (cons this (append-map (cut send <> get-all-components)
                           (get-child-components))))
  
  ; symbol (listof component<%>) -> void
  (define/public (register-children! field-name children)
    (web-cell-set! child-registry-cell 
                   (cons (cons field-name children) 
                         (web-cell-ref child-registry-cell))))
  
  ; symbol (-> (listof component<%>)) -> void
  (define/public (register-children-thunk! field-name thunk)
    (web-cell-set! child-registry-cell 
                   (cons (cons field-name thunk) 
                         (web-cell-ref child-registry-cell))))
  
  ; Returns a list of subtree components for whom (send x dirty?) returns #t.
  ; -> (listof component)
  (define/public (get-dirty-components)
    (if (dirty?)
        (list this)
        (append-map (cut send <> get-dirty-components)
                    (get-child-components))))
  
  ; Printing -----------------------------------
  
  ; output-port -> void
  (define/public (custom-write out)
    (custom-print out write (infer-class-name this)))
  
  ; output-port -> void
  (define/public (custom-display out)
    (custom-print out display (infer-class-name this)))
  
  ; output-port (any output-post -> void) (U symbol #f) -> void
  (define/public (custom-print out print class-name)
    (print (vector (or class-name 'unknown-component)
                   (with-handlers ([exn? (lambda (exn) '<no-component-id>)])
                     (get-component-id)))
           out)))

; Helpers ----------------------------------------

; -> (U symbol #f)
(define (infer-class-name obj)
  (define-values (class object-skipped?)
    (object-info obj))
  (define-values (class-name field-count field-name-list field-accessor field-mutator super-class class-skipped?) 
    (if object-skipped?
        (list #f #f #f #f #f #f #t)
        (class-info class)))
  class-name)

; any -> boolean
(define (component-list? item)
  (or (null? item)
      (and (pair? item)
           (is-a? (car item) component<%>)
           (component-list? (cdr item)))))

; Helpers ----------------------------------------

; (struct (any ... -> void) boolean)
(define-struct callback-metadata (procedure respond?) #:transparent)

; Provide statements -----------------------------

(provide component%)
