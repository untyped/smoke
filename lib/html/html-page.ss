#lang scheme

(require scheme/serialize
         (only-in (planet schematics/schemeunit:3/text-ui) display-exn)
         (planet untyped/unlib:3/enumeration)
         (planet untyped/unlib:3/list)
         (planet untyped/unlib:3/symbol)
         "../../lib-base.ss"
         "../../sizeof.ss"
         "../page.ss"
         "browser-util.ss"
         "html-component.ss"
         "html-element.ss"
         "notification.ss")

(define html-page<%>
  (interface (page<%>)
    get-doctype        ; -> xml+quotable
    set-doctype!       ; xml+quotable -> void
    get-lang           ; -> string
    set-lang!          ; string -> void
    get-title          ; -> xml+quotable
    set-title!         ; xml+quotable -> void
    make-response      ; [seed] -> response
    on-full-response   ; -> void
    on-ajax-response)) ; -> void

(define-syntax-rule (choose-rendering-mode dev?)
  (if (dev?) 'pretty 'packed))

; Logging ----------------------------------------

(define-enum smoke-response-types
  (full ajax post-redirect ajax-redirect))

; (parameter (U (url number -> void) #f))
(define frame-size-logger (make-parameter #f))

; (parameter (U (string url number -> void) #f))
(define response-time-logger (make-parameter #f))

; (_ logger any ...) -> void
(define-syntax-rule (log-frame-size)
  (let ([log (frame-size-logger)])
    (when log
      (log (request-uri (current-request))
           (sizeof (capture-web-frame))))))

(define-syntax-rule (log-response-time type expr ...)
  (let ([body (lambda () expr ...)]
        [log  (response-time-logger)])
    (if log
        (let* ([time1 (current-inexact-milliseconds)]
               [ans   (body)]
               [time2 (current-inexact-milliseconds)])
          (log type (request-uri (current-request)) (- time2 time1))
          ans)
        (body))))

; Mixins -----------------------------------------

(define-mixin html-page-mixin (page<%> html-element<%>) (html-page<%>)
  
  (inherit core-html-attributes
           get-all-components
           get-child-components
           get-component-id
           get-content-type
           get-dirty-components
           get-html-requirements/fold
           get-http-code
           get-http-status
           get-http-timestamp
           get-id
           get-js-requirements/fold)
  
  ; Fields -------------------------------------
  
  ; Optionally stores a function to use as jQuery's onError AJAX handler.
  ; The function should take four arguments and return void:
  ;
  ;     string          ; callback URL
  ;     XHR             ; the XHR used to make the request
  ;     (U string null) ; a message as to what went wrong ("timeout", "parseerror", etc)
  ;                     ;   - the second argument to jQuery's onError handler
  ;     (U exn null)    ; an exception (if any)
  ;
  ; (cell (U js (seed -> js) #f))
  (init-field ajax-error-handler
    #f
    #:accessor #:mutator)
  
  ; xml
  (init-field jquery-ui-styles
    default-jquery-ui-styles
    #:accessor #:mutator)
  
  ; (cell xml)
  (init-cell doctype
    xhtml-1.0-transitional-doctype
    #:accessor #:mutator)
  
  ; (cell string)
  (init-cell lang "en" #:accessor #:mutator)
  
  ; (cell (U string #f))
  (init-cell title #f #:accessor #:mutator)
  
  ; (cell (U string #f))
  (init-cell description #f #:accessor #:mutator)
  
  ; (cell (U string #f))
  (init-cell keywords #f #:accessor #:mutator)
  
  ; (cell string)
  (init-cell generator "Smoke by Untyped" #:accessor #:mutator)
  
  ; string
  (init [content-type "text/html; charset=utf-8"])
  
  ; (listof symbol)
  (init [classes null])
  
  ; boolean
  (init-field custom-notification-position? #f #:accessor)
  
  (super-new [classes (cons 'smoke-html-page classes)] [content-type content-type])
  
  ; notification-pane%
  (field notification-pane 
    (new notification-pane% [id (symbol-append (get-id) '-notifications)])
    #:child #:accessor)
  
  ; Accessors ----------------------------------
  
  ; -> symbol
  (define/public (get-form-id)
    (symbol-append (get-id) '-form))
  
  ; -> (listof header)
  (define/override (get-http-headers)
    no-cache-http-headers)
  
  ; Response generation ------------------------
  
  ; -> (listof (U xml (seed -> xml)))
  (define/augment (get-html-requirements)
    (let ([dev? (dev?)])
      (list* (if dev?
                 jquery-script/dev
                 jquery-script/min)
             (if dev?
                 jquery-ui-script/dev
                 jquery-ui-script/min)
             smoke-script
             jquery-ui-styles
             smoke-styles
             (inner null get-html-requirements))))
  
  ;  [#:forward? boolean] -> response
  (define/override (respond #:forward? [forward? #f])
    (unless (current-request)
      (error "No current HTTP request to respond to."))
    (current-page-set! this)
    (when forward?
      (clear-history!))
    (make-response))
  
  ; -> void
  (define/public (on-full-response)
    (void))
  
  ; -> void
  (define/public (on-ajax-response)
    (void))
  
  ; The response type varies according to the type of request being handled:
  ;   - full page requests yield complete pages of XHTML;
  ;   - AJAX requests originating from event handlers in this page yield Javascript responses
  ;     that refresh appropriate parts of the page;
  ;   - AJAX requests originating from event handlers in other pages yield Javascript responses
  ;     that redirect the browser to this page (triggering a full page refresh).
  ; [seed] -> response
  (define/public (make-response [seed (make-seed)])
    (if (ajax-request? (current-request))
        (if (equal? (ajax-request-page-id (current-request))
                    (get-component-id))
            (make-ajax-response seed)
            (make-ajax-redirect-response seed))
        (if (post-request? (current-request))
            (make-post-redirect-response seed)
            (make-full-response seed))))
  
  ; Makes a response-generator that creates a complete XHTML response for this page.
  ; [seed] -> response
  (define/public (make-full-response seed)
    (on-full-response)
    (log-frame-size)
    (parameterize ([javascript-rendering-mode (choose-rendering-mode dev?)])
      (log-response-time
       (smoke-response-types full)
       ; seed
       (let ([html-reqs (remove-duplicates (get-html-requirements/fold))]
             [js-reqs   (remove-duplicates (get-js-requirements/fold))]
             [code      (get-http-code)]
             [message   (get-http-status)]
             [seconds   (get-http-timestamp)]
             [headers   (get-http-headers)]
             [mime-type (get-content-type)]
             [content   (render seed)])
           ; response
           (make-xml-response
            #:code      code
            #:message   message
            #:seconds   seconds
            #:headers   headers
            #:mime-type mime-type
            (xml ,(get-doctype)
                 (html (@ [xmlns "http://www.w3.org/1999/xhtml"] [lang ,(get-lang)])
                       (head (meta (@ [http-equiv "Content-Type"] [content ,(get-content-type)]))
                             ,(opt-xml (get-title)
                                (title ,(get-title)))
                             ,(render-head seed)
                             ; ,@(render-requirements (get-current-html-requirements) seed)
                             ,@(render-requirements html-reqs seed)
                             (script (@ [type "text/javascript"])
                                     (!raw "\n// <![CDATA[\n")
                                     (!raw ,(js ((function ($)
                                                   (!dot ($ document)
                                                         (ready (function ()
                                                                  (!dot Smoke (initialize ,(get-component-id)
                                                                                          ,(get-form-id)
                                                                                          (function ()
                                                                                            ; Init scripts:
                                                                                            ; ,@(render-requirements (get-current-js-requirements) seed)
                                                                                            ,@(render-requirements js-reqs seed)
                                                                                            ; Attach scripts:
                                                                                            ,(get-on-attach seed))))))))
                                                 jQuery)))
                                     (!raw "\n// ]]>\n")))
                       (body (@ ,@(core-html-attributes seed)) ,content))))))))
  
  ; Makes an AJAX Javascript response that refreshes appropriate parts of this page.
  ; [seed] -> response
  (define/public (make-ajax-response seed)
    (on-ajax-response)
    (log-frame-size)
    (parameterize ([javascript-rendering-mode (choose-rendering-mode dev?)])
      (log-response-time
       (smoke-response-types ajax)
       (with-handlers ([exn? (lambda (exn)
                                 (display-exn exn)
                                 (make-js-response 
                                  #:code 500
                                  #:message "Internal Error"
                                  (js (!dot Smoke (log "An error has occurred. Talk to your system administrator.")))))])
           (make-js-response
            (js ((function ($)
                   ,@(map (cut send <> get-on-refresh seed)
                          (get-dirty-components)))
                 jQuery)))))))

  ; Response that converts a post request into a get request after form submission.
  ; This avoids "Would you like to resubmit your form?" messages when clicking the "Back" button.
  ; seed -> response
  (define/public (make-post-redirect-response seed)
    (parameterize ([javascript-rendering-mode (choose-rendering-mode dev?)])
      (log-response-time
       (smoke-response-types post-redirect)
       ; seed
       (make-html-response
        #:code      301 
        #:message   "Moved Permanently"
        #:mime-type #"text/html"
        #:headers   (cons (make-header #"Location" (string->bytes/utf-8 (embed/full seed (callback on-refresh))))
                          no-cache-http-headers)
        (xml)))))
  
  ; Makes a response that redirects the browser to this page.
  ; seed -> response
  (define/public (make-ajax-redirect-response seed)
    (parameterize ([javascript-rendering-mode (choose-rendering-mode dev?)])
      (log-response-time
       (smoke-response-types ajax-redirect)
       (make-js-response 
        (js (= (!dot window location)
               ,(embed/full seed (callback on-ajax-redirect))))))))
  
  ; -> void
  (define/public #:callback (on-ajax-redirect)
    (void))
  
  ; seed -> xml
  (define/public (render-head seed)
    (xml))
  
  ; seed -> xml
  (define/overment (render seed)
    (xml (form (@ [id             ,(get-form-id)] 
                  [class          "smoke-html-page-form"]
                  [method         "post"]
                  [enctype        "multipart/form-data"]
                  [accept-charset "utf-8"]
                  [action         "javascript:void(0)"])
               ,(opt-xml (not custom-notification-position?)
                  ,(send notification-pane render seed))
               ,(inner (xml "Page under construction.") render seed))))
  
  ; seed -> js
  (define/augment (get-on-attach seed)
    (js (!dot ($ ,(format "#~a" (get-form-id)))
              (submit (function (evt)
                        (!dot Smoke (triggerSubmitEvent #t)))))
        ,(cond [(get-ajax-error-handler)
                => (lambda (handler)
                     (if (procedure? handler)
                         (js (= (!dot Smoke onAjaxFailure) ,(handler seed)))
                         (js (= (!dot Smoke onAjaxFailure) ,handler))))]
               [else (js)])
        ,(inner (js) get-on-attach seed)))
  
  ; seed -> js
  (define/override (get-on-render seed)
    (js (!dot Smoke (insertHTML (!dot Smoke (findById ,(get-id)))
                                "children"
                                ,(xml->string (render seed))))))
  
  ; seed -> js
  (define/augment (get-on-detach seed)
    (js (!dot ($ ,(format "#~a" (get-form-id))) (unbind))
        ,(inner (js) get-on-detach seed))))

; Classes ----------------------------------------

(define-class html-page% (html-page-mixin (page-mixin html-element%)) ())

; Helpers ----------------------------------------

; (listof (U any (seed -> any))) seed -> (listof any)
(define (render-requirements reqs seed)
  (map (lambda (req)
         (if (procedure? req)
             (req seed)
             req))
       reqs))

; Provide statements -----------------------------

(provide smoke-response-types
         html-page<%>
         html-page-mixin
         html-page%)

(provide/contract
 [frame-size-logger    (parameter/c (or/c (-> url? number? any) #f))]
 [response-time-logger (parameter/c (or/c (-> (enum-value/c smoke-response-types) url? number? any) #f))])
