#lang scheme

(require "../core/env.ss"
         "class-internal.ss")

; Callback registry ------------------------------

; The complete set of generics for each Smoke class.
; Mappings for superclasses are duplicated in subclasses to make run-time lookup as cheap as possible.
; (weak-hasheqof class (immutable-hasheqof symbol (cons generic boolean)))
(define callback-registry (make-weak-hasheq))

; class symbol generic -> void
(define (register-callback! class id generic respond?)
  (letrec ([lookup (lambda (class)
                     (hash-ref callback-registry
                               class (lambda ()
                                       (let ([super (class-superclass class)])
                                         (if super
                                             (lookup super)
                                             #hasheq())))))]
           [hash   (lookup class)])
    (hash-set! callback-registry class (hash-set hash id (cons generic respond?)))))

; (U object class) symbol -> any
(define (callback-registered? object+class id)
  (let ([hash (hash-ref callback-registry
                        (if (object? object+class)
                            (object-class object+class)
                            object+class))])
    (and hash (hash-has-key? hash id))))

; (U object class) symbol -> any
(define (verify-callback object+class id)
  (unless (callback-registered? object+class id)
    (error (format "callback ~s not registered" id) object+class)))

; object symbol (listof any) -> any
(define (send-callback object id args)
  (let ([hash (hash-ref callback-registry (object-class object))])
    (if hash
        (let ([generic+respond? (hash-ref hash id)])
          (if generic+respond?
              (let ([ans (send-generic object (car generic+respond?) . args)])
                (if (cdr generic+respond?)
                    (send (current-page) respond)
                    ans))
              (error (format "callback ~s not registered" id) object)))
        (error "no callbacks registered" object))))

; Provides ---------------------------------------

(provide/contract
 [register-callback!   (-> class? symbol? generic? boolean? void?)]
 [callback-registered? (-> (or/c class? object?) symbol? boolean?)]
 [verify-callback      (-> (or/c class? object?) symbol? void?)]
 [send-callback        (-> object? symbol? list? any)])