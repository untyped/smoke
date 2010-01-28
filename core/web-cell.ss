#lang scheme

(require scheme/serialize
         srfi/26)

; Frames -----------------------------------------

; (struct (hasheqof symbol any))
(define-serializable-struct web-frame (env) #:transparent)

(define empty
  (make-web-frame #hasheq()))

; (thread-cell web-frame)
(define new-frame (make-thread-cell empty #t))
(define old-frame (make-thread-cell empty #t))

; (parameter boolean)
(define use-old-web-frame? (make-parameter #f))

; (_ expr ...)
(define-syntax-rule (with-old-web-frame expr ...)
  (parameterize ([use-old-web-frame? #t])
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

; Cells ------------------------------------------

; (struct web-cell any)
(define-serializable-struct web-cell (id default) #:transparent)

; (parameter symbol)
(define web-cell-id-prefix (make-parameter 'cell))

; any -> web-cell
(define (create-web-cell default)
  (make-web-cell (gensym (web-cell-id-prefix))
                 default))

; web-cell -> any
(define (web-cell-ref cell)
  (if (use-old-web-frame?)
      (web-cell-old cell)
      (web-cell-new cell)))

; web-cell any -> void
(define (web-cell-set! cell val)
  (web-cell-backup! cell val)
  (if (equal? val (web-cell-default cell))
      (frame-unset! new-frame cell)
      (frame-set! new-frame cell val)))

; web-cell -> boolean
(define (web-cell-changed? cell)
  (frame-set? old-frame cell))

; Helpers --------------------------------------

; web-cell -> any
(define (web-cell-new cell)
  (frame-ref new-frame cell (cut web-cell-default cell)))

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

(provide with-old-web-frame
         web-cell-id-prefix
         web-cell?
         web-frame?)

(provide/contract
 [capture-web-frame                    (-> web-frame?)]
 [update-web-frame!                    (-> web-frame? void?)]
 [rename create-web-cell make-web-cell (-> any/c web-cell?)]
 [web-cell-id                          (-> web-cell? symbol?)]
 [web-cell-ref                         (-> web-cell? any)]
 [web-cell-set!                        (-> web-cell? any/c void?)]
 [web-cell-changed?                    (-> web-cell? boolean?)])
