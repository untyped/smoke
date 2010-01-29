#lang scheme

(require scheme/serialize
         srfi/26
         (planet untyped/unlib:3/enumeration)
         "../class/undefined.ss")

; Frames -----------------------------------------

; (struct (hasheqof symbol any))
(define-serializable-struct web-frame (env) #:transparent)

; web-frame
(define empty (make-web-frame #hasheq()))

; (thread-cell web-frame)
(define base-frame (make-thread-cell empty #t))
(define new-frame  (make-thread-cell empty #t))
(define old-frame  (make-thread-cell empty #t))

; (enumof symbol)
(define-enum frame-ids (new old base))

; (parameter frame-id)
(define current-web-frame (make-parameter (frame-ids base)))

; (_ expr ...)
(define-syntax-rule (with-new-web-frame expr ...)
  (parameterize ([current-web-frame 'new])
    expr ...))

; (_ expr ...)
(define-syntax-rule (with-old-web-frame expr ...)
  (parameterize ([current-web-frame 'old])
    expr ...))

; thread-cell web-cell -> boolean
(define (frame-set? frame cell)
  (hash-has-key? (web-frame-env (thread-cell-ref frame))
                 (web-cell-id cell)))

; thread-cell web-cell any -> void
(define (frame-ref frame cell default)
  (hash-ref (web-frame-env (thread-cell-ref frame))
            (web-cell-id cell)
            default))

; thread-cell web-cell any -> void
(define (frame-set! frame cell val)
  (thread-cell-set!
   frame
   (make-web-frame
    (hash-set (web-frame-env (thread-cell-ref frame))
              (web-cell-id cell)
              val))))

; thread-cell web-cell -> void
(define (frame-unset! frame cell)
  (thread-cell-set!
   frame
   (make-web-frame
    (hash-remove (web-frame-env (thread-cell-ref frame))
                 (web-cell-id cell)))))

; -> web-frame
(define (capture-web-frame)
  (thread-cell-ref new-frame))

; web-frame -> void
(define (update-web-frame! frame)
  (thread-cell-set! new-frame frame)
  (thread-cell-set! old-frame empty))

; -> void
(define (clear-web-frame!)
  (thread-cell-set! new-frame empty)
  (thread-cell-set! old-frame empty))

; Cells ------------------------------------------

; (struct web-cell any)
(define-serializable-struct web-cell (id) #:transparent)

; (parameter symbol)
(define web-cell-id-prefix (make-parameter 'cell))

; any -> web-cell
(define (create-web-cell default)
  (let ([cell (make-web-cell (gensym (web-cell-id-prefix)))])
    (web-cell-set! cell default)
    cell))

; web-cell -> any
(define (web-cell-ref cell)
  (enum-case frame-ids (current-web-frame)
    [(old)  (web-cell-old  cell)]
    [(new)  (web-cell-new  cell)]
    [(base) (web-cell-base cell)]))

; web-cell any -> void
(define (web-cell-set! cell val)
  (enum-case frame-ids (current-web-frame)
    [(new)  (web-cell-backup! cell val)
            (if (equal? val (web-cell-base cell undefined))
                (frame-unset! new-frame cell)
                (frame-set! new-frame cell val))]
    [(old)  (if (equal? val (web-cell-new cell undefined))
                (frame-unset! old-frame cell)
                (frame-set! old-frame cell val))]
    [(base) (frame-set! base-frame cell val)]))

; web-cell -> boolean
(define (web-cell-changed? cell)
  (enum-case frame-ids (current-web-frame)
    [(new)  (frame-set? old-frame cell)]
    [(old)  (frame-set? old-frame cell)]
    [(base) #f]))

; Helpers --------------------------------------

; web-cell [any] -> any
(define (web-cell-base cell [default (cut error "no value for web cell" cell)])
  (frame-ref base-frame cell default))

; web-cell -> any
(define (web-cell-new cell [default (cut error "no value for web cell" cell)])
  (frame-ref new-frame cell (cut web-cell-base cell default)))

; web-cell -> any
(define (web-cell-old cell)
  (frame-ref old-frame cell (cut web-cell-new cell)))

; web-cell any -> void
(define (web-cell-backup! cell val)
  (let ([new (web-cell-new cell)])
    (unless (equal? val new)
      (let ([old (web-cell-old cell)])
        (if (equal? val old)
            (frame-unset! old-frame cell)
            (unless (frame-set? old-frame cell)
              (frame-set! old-frame cell new)))))))

; Provide statements ---------------------------

(provide with-new-web-frame
         with-old-web-frame
         web-cell-id-prefix
         web-cell?
         web-frame?)

(provide/contract
 [capture-web-frame                    (-> web-frame?)]
 [update-web-frame!                    (-> web-frame? void?)]
 [clear-web-frame!                     (-> void?)]
 [rename create-web-cell make-web-cell (-> any/c web-cell?)]
 [web-cell-id                          (-> web-cell? symbol?)]
 [web-cell-ref                         (-> web-cell? any)]
 [web-cell-set!                        (-> web-cell? any/c void?)]
 [web-cell-changed?                    (-> web-cell? boolean?)])
