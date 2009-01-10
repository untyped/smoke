#lang scheme/base

(require (planet untyped/unlib:3/symbol)
         "../lib-base.ss")

; Interfaces -------------------------------------

(define component<%>
  (interface (object/cells<%>)
    get-component-id       ; -> symbol
    on-request             ; request -> void
    render                 ; seed -> content
    get-child-components   ; -> (listof component<%>)
    get-all-components     ; -> (listof component<%>)
    get-dirty-components)) ; -> (listof component<%>)

; Classses ---------------------------------------

(define component%
  (class/cells object/cells% (component<%>)
    
    (inherit dirty?)
    
    ; Fields -------------------------------------
    
    ; (cell symbol)
    (init-field [component-id (gensym/interned 'smoke)]
      #:accessor)
    
    ; (cell (alistof symbol (-> (listof component<%>)))
    (cell [child-registry null])
    
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
    
    ; seed -> content
    ;
    ; Returns the visible content for this component and its subtree.
    ; Must be overridden in subclasses of component%.
    (define/public (render seed)
      (error "render: must be overridden in a subclass of component%."))
    
    ; Children -----------------------------------
    
    ; -> (listof component<%>)
    ;
    ; Returns a list of all child components, whether they are attached or not.
    (define/public (get-child-components)
      (append-map (match-lambda
                    [(list-rest field-name thunk)
                     (with-handlers
                         ([exn? (lambda (exn)
                                  (raise (make-exn:fail
                                          (format "Bad child field: ~a: ~a" field-name (exn-message exn))
                                          (exn-continuation-marks exn))))])
                       (let ([children (thunk)])
                         (if (component-list? children)
                             children
                             (error (format "Expected (listof component<%>), received ~s" children)))))])
                  (with-handlers
                      ([exn? (lambda (exn)
                               (error "No child registry found: " this))])
                    (web-cell-ref child-registry-cell))))
    
    ; symbol -> (U component<%> #f)
    (define/public (find-component/id id)
      (or (and (eq? id (get-component-id)) this)
          (ormap (cut send <> find-component/id id)
                 (get-child-components))))
    
    ; -> (listof component<%>)
    ;
    ; Returns a list of all components in the subtree of this component
    ; (attached or unattached; including this component itself).
    (define/public (get-all-components)
      (cons this (append-map (cut send <> get-all-components)
                             (get-child-components))))
    
    ; symbol (-> (listof component<%>)) -> void
    (define/public (register-children-thunk! field-name thunk)
      (web-cell-set! child-registry-cell 
                     (cons (cons field-name thunk) 
                           (web-cell-ref child-registry-cell))))
    
    ; Dirtiness ----------------------------------
    
    ; -> (listof component)
    ;
    ; Returns a list of subtree components for whom (send x dirty?) returns #t.
    (define/public (get-dirty-components)
      (if (dirty?)
          (list this)
          (append-map (cut send <> get-dirty-components)
                      (get-child-components))))))

; Procedures -------------------------------------

; any -> boolean
(define (component? item)
  (is-a? item component<%>))

; any -> boolean
(define (component-list? item)
  (or (null? item)
      (and (pair? item)
           (component? (car item))
           (component-list? (cdr item)))))

; Provide statements -----------------------------

(provide component<%>
         component%
         component?)
