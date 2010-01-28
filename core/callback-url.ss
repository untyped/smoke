#lang scheme

(require net/url
         srfi/26
         web-server/http
         (planet untyped/unlib:3/debug)
         "callback.ss"
         "env.ss"
         "interfaces.ss"
         "json.ss")

(define-syntax-rule (debug-path msg fn arg ...)
  (let ([p (fn arg ...)])
    (debug msg (map path/param-path p))
    p))

; Procedures -------------------------------------

; seed callback -> string
(define (callback-url seed callback)
  (url->string
   (make-url #f #f #f #f #t
             (append (url-path-base (request-uri (current-request)))
                     (map (cut make-path/param <> null)
                          (list* "_"
                                 (symbol->string (send (callback-component callback) get-component-id))
                                 (symbol->string (send (callback-component callback) verify-callback-id (callback-callback-id callback)))
                                 (map (lambda (arg)
                                        (if (symbol? arg)
                                            (if (memq arg '(true false null))
                                                (error "Cannot serialize the symbols 'true, 'false or 'null in a callback URL.")
                                                (symbol->string arg))
                                            (scheme->json arg)))
                                      (callback-args callback)))))
             null #f)))

; request page -> (U callback #f)
(define (request->callback request page)
  (match (url-path-extension (request-uri request))
    [(list component-id-element ; path/param
           callback-id-element  ; path/param
           arg-elements ...)    ; (listof path/param)
     (let ([component-id (string->symbol (path/param-path component-id-element))]
           [callback-id  (string->symbol (path/param-path callback-id-element))]
           [args         (map (lambda (path/param)
                                (let ([path (path/param-path path/param)])
                                  (with-handlers ([exn? (lambda _ (string->symbol path))])
                                    (json->scheme path))))
                              arg-elements)])
       (make-callback (send page find-component/id component-id)
                      callback-id
                      args))]
    [#f #f]))

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
        [(list "") null]
        [(list _ ... "_" ext ...)
         (for/list ([str (in-list ext)])
           (make-path/param str null))]
        [_ null])
      (raise-type-error 'url-path-extension "absolute-url" url)))

; Provide statements -----------------------------

(provide/contract
 [callback-url       (-> seed? callback? string?)]
 [request->callback  (-> request? (is-a?/c page<%>) (or/c callback? #f))]
 [url-path-base      (-> (listof path/param?) (listof path/param?))]
 [url-path-extension (-> (listof path/param?) (or/c (listof path/param?) #f))])
