#lang scheme/base

(require net/url
         scheme/contract
         web-server/http/request-structs)

; Accessors --------------------------------------

; request -> boolean
(define (get-request? request)
  (let ([method (request-method request)])
    (or (and (symbol? method) (eq? method 'get))
        (and (bytes? method) (equal? method #"GET")))))

; request -> boolean
(define (head-request? request)
  (let ([method (request-method request)])
    (or (and (symbol? method) (eq? method 'head))
        (and (bytes? method) (equal? method #"HEAD")))))

; request -> boolean
(define (post-request? request)
  (let ([method (request-method request)])
    (or (and (symbol? method) (eq? method 'post))
        (and (bytes? method) (equal? method #"POST")))))

; request -> boolean
(define (put-request? request)
  (let ([method (request-method request)])
    (or (and (symbol? method) (eq? method 'put))
        (and (bytes? method) (equal? method #"PUT")))))

; request symbol -> (U string #f)
(define (request-binding-ref request key)
  (or (request-binding-ref/internal request key)
      (ormap (lambda (pair)
               (and (eq? key (car pair))
                    (cdr pair)))
             (url-query (request-uri request)))))

; request symbol -> (U string #f)
(define (request-get-binding-ref request key)
  (and (get-request? request)
       (request-binding-ref/internal request key)))

; request symbol -> (U string #f)
(define (request-post-binding-ref request key)
  (and (post-request? request)
       (request-binding-ref/internal request key)))

; request symbol -> (U string #f)
(define (request-upload-filename-ref request key)
  (define key-bytes (string->bytes/utf-8 (symbol->string key)))
  (and (post-request? request)
       (ormap (lambda (binding)
                (and (equal? key-bytes (binding-id binding))
                     (if (binding:file? binding)
                         (bytes->string/utf-8 (binding:file-filename binding))
                         (error (format "~a is bound to a normal form value." key)))))
              (request-bindings/raw request))))

; request symbol -> (U bytes #f)
(define (request-upload-content-ref request key)
  (define key-bytes (string->bytes/utf-8 (symbol->string key)))
  (and (post-request? request)
       (ormap (lambda (binding)
                (and (equal? key-bytes (binding-id binding))
                     (if (binding:file? binding)
                         (binding:file-content binding)
                         (error (format "~a is bound to a normal form value." key)))))
              (request-bindings/raw request))))

; request (symbol string -> ans) -> (listof ans)
(define (request-binding-map request fn)
  ; (listof ans)
  (define binding-values
    (foldl (lambda (binding accum)
             (if (binding:form? binding)
                 (cons (fn (string->symbol (bytes->string/utf-8 (binding-id binding)))
                           (bytes->string/utf-8 (binding:form-value binding)))
                       accum)
                 accum))
           null
           (request-bindings/raw request)))
  ; (listof ans)
  (if (post-request? request)
      (reverse (foldl (lambda (kvp accum)
                        (cons (fn (car kvp) (cdr kvp)) accum))
                      binding-values
                      (url-query (request-uri request))))
      (reverse binding-values)))

; Helpers ----------------------------------------

; request symbol -> (U string #f)
(define (request-binding-ref/internal request key)
  ; bytes
  (define key-bytes (string->bytes/utf-8 (symbol->string key)))
  (ormap (lambda (binding)
           (and (equal? key-bytes (binding-id binding))
                (if (binding:form? binding)
                    (bytes->string/utf-8 (binding:form-value binding))
                    (error (format "~a is bound to an uploaded file." key)))))
         (request-bindings/raw request)))

; Provide statements -----------------------------

(provide/contract
 [get-request?                (-> request? boolean?)]
 [head-request?               (-> request? boolean?)]
 [post-request?               (-> request? boolean?)]
 [put-request?                (-> request? boolean?)]
 [request-binding-ref         (-> request? symbol? (or/c string? #f))]
 [request-get-binding-ref     (-> request? symbol? (or/c string? #f))]
 [request-post-binding-ref    (-> request? symbol? (or/c string? #f))]
 [request-upload-filename-ref (-> request? symbol? (or/c string? #f))]
 [request-upload-content-ref  (-> request? symbol? (or/c bytes?  #f))]
 [request-binding-map         (-> request? (-> symbol? string? any) list?)])
