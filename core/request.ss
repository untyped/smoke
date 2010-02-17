#lang scheme

(require net/url
         scheme/contract
         web-server/http/bindings
         web-server/http/request-structs)

; Request methods --------------------------------

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

; AJAX headers -----------------------------------

; request -> boolean
(define (ajax-request? request)
  ; The requested-with clause should match all AJAX requests sent by JQuery,
  ; but we include the page-id clause anyway for good measure:
  (and (or (ajax-request-requested-with request)
           (ajax-request-page-id request))
       #t))

; request -> (U string #f)
(define (ajax-request-requested-with request)
  ; (U string #f)
  (ormap (lambda (pair)
           (and (or (equal? (car pair) 'x-requested-with)
                    (equal? (car pair) 'X-Requested-With))
                (cdr pair)))
         (request-headers request)))

; request -> (U symbol #f)
(define (ajax-request-page-id request)
  ; (U symbol #f)
  (ormap (lambda (pair)
           (and (or (equal? (car pair) 'x-smoke-page)
                    (equal? (car pair) 'X-Smoke-Page))
                (string->symbol (cdr pair))))
         (request-headers request)))

; Headers ----------------------------------------

; request symbol -> (U string #f)
(define (request-header-ref request key)
  (let ([key-bytes (string->bytes/utf-8 (symbol->string key))])
    (ormap (lambda (header)
             (and (equal? key-bytes (header-field header))
                  (bytes->string/utf-8 (header-value header))))
           (request-headers/raw request))))

; Bindings ---------------------------------------

; request symbol -> (U string #f)
(define (request-binding-ref request key)
  (or (request-post-binding-ref request key)
      (request-get-binding-ref request key)))

; request symbol -> (U string #f)
(define (request-get-binding-ref request key)
  (ormap (lambda (pair)
           (and (eq? key (car pair))
                (cdr pair)))
         (url-query (request-uri request))))

; request symbol -> (U string #f)
(define (request-post-binding-ref request key)
  (and (post-request? request)
       (let ([key-bytes (string->bytes/utf-8 (symbol->string key))])
         (ormap (lambda (binding)
                  (and (equal? key-bytes (binding-id binding))
                       (if (binding:form? binding)
                           (bytes->string/utf-8 (binding:form-value binding))
                           (error (format "~a is bound to an uploaded file." key)))))
                (request-bindings/raw request)))))

; request symbol -> (U string #f)
(define (request-upload-filename-ref request key)
  (and (post-request? request)
       (let ([key-bytes (string->bytes/utf-8 (symbol->string key))])
         (ormap (lambda (binding)
                  (and (equal? key-bytes (binding-id binding))
                       (if (binding:file? binding)
                           (bytes->string/utf-8 (binding:file-filename binding))
                           (error (format "~a is bound to a normal form value." key)))))
                (request-bindings/raw request)))))

; request symbol -> (U bytes #f)
(define (request-upload-content-ref request key)
  (and (post-request? request)
       (let ([key-bytes (string->bytes/utf-8 (symbol->string key))])
         (ormap (lambda (binding)
                  (and (equal? key-bytes (binding-id binding))
                       (if (binding:file? binding)
                           (binding:file-content binding)
                           (error (format "~a is bound to a normal form value." key)))))
                (request-bindings/raw request)))))

; Provide statements -----------------------------

(provide/contract
 [get-request?                (-> request? boolean?)]
 [head-request?               (-> request? boolean?)]
 [post-request?               (-> request? boolean?)]
 [put-request?                (-> request? boolean?)]
 [ajax-request?               (-> request? boolean?)]
 [ajax-request-requested-with (-> request? (or/c string? #f))]
 [ajax-request-page-id        (-> request? (or/c symbol? #f))]
 [request-header-ref          (-> request? symbol? (or/c string? #f))]
 [request-binding-ref         (-> request? symbol? (or/c string? #f))]
 [request-get-binding-ref     (-> request? symbol? (or/c string? #f))]
 [request-post-binding-ref    (-> request? symbol? (or/c string? #f))]
 [request-upload-filename-ref (-> request? symbol? (or/c string? #f))]
 [request-upload-content-ref  (-> request? symbol? (or/c bytes?  #f))])
