#lang scheme/base

(require scheme/contract
         scheme/pretty
         scheme/serialize
         srfi/26
         (planet untyped/unlib:3/debug)
         (planet untyped/unlib:3/exn))

; Frame structure type ---------------------------

; (struct symbol (U frame #f) namespace)
(define-struct frame (id parent namespace)
  #:transparent
  #:property prop:custom-write
  (lambda (frame out write?)
    (let ([show (if write? write display)])
      (show (vector 'struct:frame (frame-id frame)) out))))

; frame -> any
(define (frame-serialize frame)
  (serialize (list* (frame-id frame) 
                    (and (frame-parent frame) (frame-id (frame-parent frame)))
                    (map (lambda (id)
                           (cons id (frame-ref frame id)))
                         (namespace-mapped-symbols (frame-namespace frame))))))

; (parameter (U symbol #f))
; Used in create-web-cell to provide a more meaningful cell ID.
(define web-cell-id-prefix
  (make-parameter #f))

; Frames -----------------------------------------

; (U frame #f) -> frame
(define (create-frame parent)
  (make-frame (gensym 'f) parent (make-empty-namespace)))

; [frame] -> frame
;
; Creates a new frame at the top of the stack.
(define (push-frame [frame (current-frame)])
  (create-frame frame))

; frame -> boolean
(define (frame-has-parent? frame)
  (if (frame-parent frame) #t #f))

; frame -> frame | exn:fail
(define (frame-parent/exn frame [var #f])
  (or (frame-parent frame) 
      (error "Reached top of stack")))

; variables/namespace

; frame symbol -> any
(define (frame-ref frame var)
  (unless frame (raise-exn exn:fail:contract "Expected frame, received #f."))
  (namespace-variable-value 
   var
   #f
   (cut frame-ref (frame-parent/exn frame) var)
   (frame-namespace frame)))

; frame symbol -> boolean
(define (frame-set? frame var)
  (unless frame (raise-exn exn:fail:contract "Expected frame, received #f."))
  (let ([defined? #t])
    (namespace-variable-value var #f (cut set! defined? #f) (frame-namespace frame))
    defined?))

; frame symbol any -> void
(define (frame-set! frame var val)
  (unless frame (raise-exn exn:fail:contract "Expected frame, received #f."))
  (namespace-set-variable-value! var val #t (frame-namespace frame)))

; frame symbol -> void
(define (frame-unset! frame var)
  (unless frame (raise-exn exn:fail:contract "Expected frame, received #f."))
  (with-handlers ([exn? void])
    (namespace-undefine-variable! var (frame-namespace frame))))

; frame -> void
;
; Moves all variable bindings from a child frame to its parent. Existing
; bindings in the parent frame are overridden and all bindings in the
; child are removed.
;
; Raises exn:fail:contract if the supplied frame has no parent.
(define (frame-squeeze! frame)
  (unless (and frame (frame-parent frame))
    (raise-exn exn:fail:contract (format "Expected frame with parent, received ~s" frame)))
  (let* ([parent    (frame-parent frame)]
         [child-ns  (frame-namespace frame)]
         [parent-ns (frame-namespace parent)])
    (for-each (lambda (var)
                (define val (namespace-variable-value var #f #f child-ns))
                (namespace-set-variable-value! var val #t parent-ns)
                (namespace-undefine-variable! var child-ns))
              (namespace-mapped-symbols child-ns))))

; Frame stack ----------------------------------

; frame (frame -> boolean) -> (U frame #f)
#;(define (search-frames frame predicate?)
    (cond [(predicate? frame)   frame]
          [(frame-parent frame) (search-frames (frame-parent frame) predicate?)]
          [else                 #f]))

; frame -> boolean
(define (frame-root? frame)
  (not (frame-parent frame)))

; frame -> frame
(define (frame-root frame)
  (if (frame-parent frame)
      (frame-root (frame-parent frame))
      frame))

; frame -> (listof symbol)
(define (frame-debug frame)
  (if frame
      (cons (frame-id frame) (frame-debug (frame-parent frame)))
      null))

; Current frame --------------------------------

; (thread-cell frame)
(define current-frame
  (let ([root-frame (create-frame #f)])
    (make-parameter root-frame)))

; frame (-> any) -> any
(define (call-with-frame frame thunk)
  (parameterize ([current-frame frame])
    (thunk)))

; Cells ----------------------------------------

; (parameter boolean)
(define print-web-cell
  (make-parameter #f))

; (struct symbol)
(define-struct web-cell (id)
  #:property prop:custom-write
  (lambda (id out write?)
    (short-write id out write?))
  #:transparent)

; web-cell output-port boolean -> void
(define (debug-write item out write?)
  (let* ([id    (web-cell-id item)]
         [show  (if write? write display)]
         [vals  (let loop ([frame (current-frame)] [accum null])
                  (if frame
                      (loop (frame-parent frame)
                            (cons (namespace-variable-value
                                   id
                                   #f
                                   (lambda _ '_)
                                   (frame-namespace frame))
                                  accum))
                      (reverse accum)))])
    (show (vector 'struct:web-cell id vals) out)))

; web-cell output-port boolean -> void
(define (short-write item out write?)
  (let ([id   (web-cell-id item)]
        [show (if write? write display)])
    (show (vector 'struct:web-cell id) out)))

; any -> web-cell
(define (create-web-cell initial)
  (define id (gensym (or (web-cell-id-prefix) 'c)))
  (frame-set! (current-frame) id initial)
  (make-web-cell id))

; web-cell [frame] -> any
(define (web-cell-ref cell [frame (current-frame)])
  (define id (web-cell-id cell))
  (with-handlers ([exn:fail:contract? (lambda (exn) (raise exn))]
                  [exn:fail?          (lambda (exn)
                                        (raise-exn exn:fail
                                          (format "web-cell-ref ~a: ~a"
                                                  id
                                                  (exn-message exn))))])
    (frame-ref frame id)))

; web-cell [frame] [(any any -> boolean)] -> any
;
; Returns #t if the web cell has been changed in the current frame.
; 
; More formally, returns #t if the supplied cell has a value in the
; current frame and:
;   - EITHER the cell does not have a value in any parent frame
;   - OR the cell has a value in a parent frame but the two values
;     are not equal (according to the supplied predicate)
(define (web-cell-changed? cell [frame (current-frame)] [same? equal?])
  (with-handlers ([exn:fail:contract? (lambda (exn) (raise exn))]
                  [exn:fail?          (lambda (exn)
                                        (raise-exn exn:fail
                                          (format "web-cell-changed? ~a ~a: ~a" 
                                                  (web-cell-id cell)
                                                  (frame-id frame)
                                                  (exn-message exn))))])
    (let* ([id (web-cell-id cell)]
           [value1 (frame-ref frame id)])
      (if (frame-parent frame)
          (with-handlers ([exn:fail? (lambda (exn) #t)])
            (let ([value2 (frame-ref (frame-parent frame) id)])
              (not (same? value1 value2))))
          #t))))

; web-cell [frame] -> boolean
(define (web-cell-set? cell [frame (current-frame)])
  (with-handlers ([exn:fail:contract? (lambda (exn) (raise exn))]
                  [exn:fail?          (lambda (exn)
                                        (raise-exn exn:fail
                                          (format "web-cell-set? ~a: ~a" 
                                                  (web-cell-id cell)
                                                  (exn-message exn))))])
    (frame-set? frame (web-cell-id cell))))

; cell any [frame] -> void
(define (web-cell-set! cell val [frame (current-frame)])
  (define id (web-cell-id cell))
  (frame-set! frame id val))

; cell [frame] -> void
(define (web-cell-unset! cell [frame (current-frame)])
  (define id (web-cell-id cell))
  (frame-unset! frame id))

; web-cell any -> void
#;(define (web-cell-set*! cell val)
    (define id (web-cell-id cell))
    (frame-set! (search-frames (current-frame) (cut frame-set? <> id))
                id val))

; web-cell [frame] -> list
(define (web-cell-debug cell [frame (current-frame)])
  (if frame
      (cons (list (frame-id frame) 
                  (namespace-variable-value (web-cell-id cell) #f void (frame-namespace frame)))
            (web-cell-debug cell (frame-parent frame)))
      null))

; [frame] -> sequence
(define (in-frames [frame (current-frame)])
  (make-do-sequence
   (lambda ()
     (values
      ; pos->element : position -> (U frame #f)
      (lambda (pos)
        pos)
      ; next-pos : position -> position
      (lambda (pos)
        (frame-parent pos))
      ; position
      frame
      ; sequence-still-going? : position -> boolean
      (lambda (pos)
        pos)
      ; sequence-still-going? : (U frame #f) -> boolean
      (lambda (pos)
        pos)
      ; sequence-still-going? : position (U frame #f) -> boolean
      (lambda (pos frame)
        pos))))) 

; Provide statements ---------------------------

(provide/contract
 ; Frames:
 [push-frame                           (->* () (frame?) frame?)]
 [frame?                               (-> any/c boolean?)]
 [frame-has-parent?                    (-> frame? boolean?)]
 [frame-parent                         (-> frame? (or/c frame? false/c))]
 [frame-parent/exn                     (-> frame? frame?)]
 [frame-id                             (-> frame? symbol?)]
 [frame-ref                            (-> frame? symbol? any)]
 [frame-set?                           (-> frame? symbol? boolean?)]
 [frame-set!                           (-> frame? symbol? any/c any)]
 [frame-unset!                         (-> frame? symbol? any)]
 [frame-squeeze!                       (-> frame? any)]
 #;[search-frames                        (-> frame? procedure? frame?)]
 [frame-root?                          (-> frame? boolean?)]
 [frame-root                           (-> frame? frame?)]
 [frame-debug                          (-> frame? (listof symbol?))]
 [frame-serialize                      (-> frame? any)]
 ; Current frame:
 [current-frame                        (parameter/c frame?)]
 [call-with-frame                      (-> frame? procedure? any)]
 ; Cells:
 [print-web-cell                       (parameter/c boolean?)]
 [web-cell-id-prefix                   (parameter/c (or/c symbol? false/c))]
 [rename create-web-cell make-web-cell (-> any/c web-cell?)]
 [web-cell?                            (-> any/c boolean?)]
 [web-cell-id                          (-> web-cell? any)]
 [web-cell-ref                         (->* (web-cell?) (frame?) any)]
 [web-cell-changed?                    (->* (web-cell?) (frame? procedure?) any)]
 [web-cell-set?                        (->* (web-cell?) (frame?) boolean?)]
 [web-cell-set!                        (->* (web-cell? any/c) (frame?) void?)]
 [web-cell-unset!                      (->* (web-cell?) (frame?) void?)]
 #;[web-cell-set*!                       (-> web-cell? any/c void?)]
 [web-cell-debug                       (->* (web-cell?) (frame?) list?)]
 [in-frames                            (->* () (frame?) sequence?)])
