#lang scheme

(require net/url
         srfi/26
         web-server/http
         (planet untyped/unlib:3/debug)
         (planet untyped/unlib:3/for)
         "callback.ss"
         "env.ss"
         "interfaces.ss"
         "json.ss"
         "request.ss"
         "web-cell.ss")

(define-syntax-rule (debug-path msg fn arg ...)
  (let ([p (fn arg ...)])
    (debug msg (map path/param-path p))
    p))

; Procedures -------------------------------------

; url -> boolean
(define (initial-url? url)
  (or (not (url-path-absolute? url))
      (and (not (url-query-ref url '__k))
           (match (map path/param-path (url-path url))
             [(list _ ... "_" _ ...) #f]
             [_ #t]))))

; url -> boolean
(define (callback-url? url)
  (and (url-path-absolute? url)
       (url-query-ref url '__k)
       (match (map path/param-path (url-path url))
         [(list _ ... "_" _ _ _ ...) #t]
         [_ #f])))

; seed callback [(alistof symbol string)] -> string
(define (callback->url seed callback [bindings null])
  (url->string
   (make-url
    #f #f #f #f #t
    (append (url-path-base (request-uri (current-request)))
            (map (cut make-path/param <> null)
                 (list* "_"
                        (symbol->string (callback-component-id callback))
                        (symbol->string (callback-method-id    callback))
                        (map (lambda (arg)
                               (if (symbol? arg)
                                   (if (memq arg '(true false null))
                                       (error "Cannot serialize the symbols 'true, 'false or 'null in a callback URL.")
                                       (symbol->string arg))
                                   (scheme->json arg)))
                             (callback-args callback)))))
    (cons (cons '__k (current-callback-serial)) bindings)
    #f)))

; url -> url
(define (url->initial url)
  (if (url-path-absolute? url)
      (make-url (url-scheme url)
                (url-user url)
                (url-host url)
                (url-port url)
                #t
                (url-path-base url)
                (for/filter ([kvp (in-list (url-query url))])
                  (and (not (eq? (car kvp) '__k))
                       kvp))
                (url-fragment url))
      (raise-type-error 'url->initial-url "absolute-url" url)))

; url site<%> -> callback
(define (url->callback url app)
  (match (map path/param-path (url-path-extension url))
    [(list (app string->symbol component-id)
           (app string->symbol method-id)
           args ...)
       (make-callback
        component-id
        method-id
        (for/list ([arg (in-list args)])
          (with-handlers ([exn? (lambda _ (string->symbol arg))])
            (json->scheme arg))))]
    [_ (error "malformed callback")]))

; (listof path/param) -> (listof path/param)
(define (url-path-base url)
  (if (url-path-absolute? url)
      (match (map path/param-path (url-path url))
        [(list "") null]
        [(or (list base ... "_" _ ...)
             (list base ...))
         (for/list ([str (in-list base)])
           (make-path/param str null))])
      (raise-type-error 'url-path-base "absolute-url" url)))

; (listof path/param) -> (U (listof path/param) #f)
(define (url-path-extension url)
  (if (url-path-absolute? url)
      (match (map path/param-path (url-path url))
        [(list _ ... "_" ext ...)
         (for/list ([str (in-list ext)])
           (make-path/param str null))]
        [_ #f])
      (raise-type-error 'url-path-extension "absolute-url" url)))

; url symbol -> (U string #f)
(define (url-query-ref url key)
  (ormap (lambda (pair)
           (and (eq? key (car pair))
                (cdr pair)))
         (url-query url)))

; [request] -> (U string #f)
(define (request-serial [request (current-request)])
  (or (request-binding-ref request '__k)
      (error "no serial in request" request)))

; Provide statements -----------------------------

(provide/contract
 [initial-url?            (-> url? boolean?)]
 [callback-url?           (-> url? boolean?)]
 [callback->url           (-> seed? callback? string?)]
 [url->initial            (-> url? url?)]
 [url->callback           (-> url? (is-a?/c site<%>) callback?)]
 [url-path-base           (-> url? (listof path/param?))]
 [url-path-extension      (-> url? (or/c (listof path/param?) #f))]
 [url-query-ref           (-> url? symbol? (or/c string? #f))]
 [request-serial          (->* () (request?) string?)])
