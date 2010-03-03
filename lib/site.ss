#lang scheme

(require file/md5
         (only-in web-server/dispatchers/dispatch next-dispatcher)
         "../lib-base.ss"
         "component.ss"
         "page.ss"
         "dispatch/codec.ss"
         "dispatch/core.ss")

(define-class site% component% (site<%>)
  
  ; Fields ---------------------------------------
  
  ; (listof (box rule))
  (init-field rules #:accessor)
  
  ; (listof (box page<%>))
  (init-field page-boxes null)
  
  ; Requests and responses -----------------------
  
  ; -> response
  (define/public (dispatch)
    (let* ([request (current-request)]
           [url     (request-uri request)])
      (cond [(initial-url?  url) (dispatch-initial url)]
            [(callback-url? url) (init-web-frame)
                                 (begin0
                                   (dispatch-callback (url->callback url this))
                                   (save-web-frame))]
            [else                (make-redirect-response (url->initial url))])))
  
  ; url -> response
  (define/public (dispatch-initial url)
    (with-handlers ([exn:fail:dispatch? (lambda _ (not-found))])
      (match (decode-url url)
        [(list-rest page args) 
         (init-web-frame)
         (begin0
           (send/apply page dispatch-initial args)
           (save-web-frame))]
        [#f (next-dispatcher)])))
  
  ; callback -> response
  (define/public (dispatch-callback callback)
    (let-values/debug ([(page comp) (find-page+component (debug* "id" callback-component-id callback))])
      (current-page-set! page)
      (send/apply page dispatch-callback
                  comp
                  (callback-method-id callback)
                  (callback-args callback))))
    
  ; -> response
  (define/public (not-found)
    (make-html-response
     #:code 404
     (xml (html (body "Not found")))))
  
  ; page<%> any ... -> response
  (define/public (page-undefined page . args)
    (make-html-response
     #:code 404
     (xml (html (body "Page undefined")))))
    
  ; Decoding/encoding URLs -----------------------
  
  ; url -> (U (list page<%> any ...) #f)
  (define/public (decode-url url)
    (let ([url-string (url->string url)])
      (for/or ([rule (in-list rules)])
        (let ([match (pattern-decode (rule-pattern rule) url-string)])
          (and match (cons (unbox (rule-page-box rule)) match))))))
  
  ; page<%> (listof any) -> string
  (define/public (encode-url page args)
    (or (for/or ([rule (in-list rules)])
          (and (eq? (unbox (rule-page-box rule)) page)
               (pattern-encode (rule-pattern rule) args)))
        (error "no url for page + arguments" (cons page args))))
  
  ; Pages and components -------------------------
  
  ; -> (listof page<%>)
  (define/public (get-pages)
    (map unbox page-boxes))

  ; This method is only necessary for legacy code where pages aren't being defined with define-page.
  ; page<%> -> void
  (define/public (add-page! page)
    ; Prevent duplicates:
    (unless (ormap (cut eq? <> page) (get-pages))
      (set! page-boxes (cons (box page) page-boxes))))
  
  ; symbol -> (U page<%> #f) (U component<%> #f)
  (define/public (find-page+component id)
    (let loop ([pages (get-pages)])
      (match pages
        [(list) (values #f #f)]
        [(list-rest #f rest)
         (loop rest)]
        [(list-rest page rest)
         (let/debug ([comp (send page find-component id)])
           (if comp
               (values page comp)
               (loop rest)))]))))

; Provides ---------------------------------------

(provide site%)
