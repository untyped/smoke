#lang scheme/base

(require (for-syntax scheme/base)
         (only-in srfi/1 delete delete-duplicates)
         (only-in (planet schematics/schemeunit:3/text-ui) display-exn)
         (planet untyped/unlib:3/enumeration)
         (planet untyped/unlib:3/list)
         (planet untyped/unlib:3/symbol)
         "../../lib-base.ss"
         "../../sizeof.ss"
         "../../web-server/expired-continuation.ss"
         "../../web-server/resume.ss"
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
    on-full-response   ; -> void
    on-ajax-response)) ; -> void

(define-syntax-rule (choose-rendering-mode dev?)
  (if (dev?) 'pretty 'packed))

; Logging ----------------------------------------

(define-enum smoke-response-types
  (full ajax full-redirect ajax-redirect))

; (parameter (U (url number -> void) #f))
(define frame-size-logger (make-parameter #f))

; (parameter (U (string url number -> void) #f))
(define response-time-logger (make-parameter #f))

; (_ logger any ...) -> void
(define-syntax-rule (log-frame-size)
  (let ([log (frame-size-logger)])
    (when log
      (log (request-uri (current-request))
           (sizeof (current-frame))))))

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

(define html-page-mixin
  (mixin/cells (page<%> html-element<%>) (html-page<%>)
    
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
    (init-field ajax-error-handler #f #:accessor #:mutator)
    
    ; (cell xml)
    (init-cell doctype
      xhtml-1.0-transitional-doctype
      #:accessor #:mutator)
    
    ; (cell string)
    (init-cell lang "en" #:accessor #:mutator)
    
    ; (cell (U string #f))
    (init-cell title #f #:accessor #:mutator)
    
    ; (cell string)
    (init-cell description #f #:accessor #:mutator)
    
    ; (cell string)
    (init-cell keywords #f #:accessor #:mutator)
    
    ; (cell string)
    (init-cell generator "Smoke by Untyped" #:accessor #:mutator)
    
    ; (cell (U (list integer integer integer) #f))
    (cell callback-codes #f #:accessor #:mutator)
    
    ; (cell (listof (U xml (seed -> xml))))
    (cell current-html-requirements null #:accessor #:mutator)
    
    ; (cell (listof (U js (seed -> js))))
    (cell current-js-requirements null #:accessor #:mutator)
    
    ; string
    (init [content-type "text/html; charset=utf-8"])
    
    ; (listof symbol)
    (init [classes null])
    
    ; jquery-version
    (init [jquery-version (jquery-versions 1.3.2)])
    
    ; jquery-ui-version
    (init [jquery-ui-version (jquery-ui-versions 1.7.1)])
    
    ; string
    (init [jquery-ui-theme "ui-lightness"])
    
    ; xml
    (field jquery-script
      (if dev?
          (jquery-script/dev jquery-version)
          (jquery-script/min jquery-version)))
    
    ; xml
    (field jquery-ui-script
      (if dev?
          (jquery-ui-script/dev jquery-ui-version)
          (jquery-ui-script/min jquery-ui-version)))
    
    ; xml
    ; Specify a jquery-ui-theme of #f if you don't want html-page to add a stylesheet for you.
    (field jquery-ui-stylesheet
      (opt-xml jquery-ui-theme
        ,(jquery-ui-styles jquery-ui-version jquery-ui-theme)))
    
    ; boolean
    (init-field custom-notification-position? #f #:accessor)
    
    (super-new [classes (cons 'smoke-html-page classes)] [content-type content-type])
    
    ; notification-pane%
    (field notification-pane 
      (new notification-pane% [id 'notification-pane])
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
      (list* jquery-script
             jquery-ui-script
             smoke-script
             jquery-ui-stylesheet
             smoke-styles
             (inner null get-html-requirements)))
    
    ;  [#:forward? boolean] -> any
    (define/override (respond #:forward? [forward? #f])
      (define (actually-respond)
        (let ([push-frame? (and (not (ajax-request? (current-request)))
                                (not (post-request? (current-request))))])
          (when forward? (clear-continuation-table!))
          (parameterize ([current-page this])
            (when push-frame?
              (resume-from-here))
            (send/suspend/dispatch (make-response-generator #:forward? forward?) #:push-frame? push-frame?))))
      
      (unless (current-request)
        (error "No current HTTP request to respond to."))
      
      (when (expired-continuation-type)
        (notifications-add! (expired-continuation-xml (expired-continuation-type)))
        (expired-continuation-type-reset!))
      
      (if (resume-available?)
          (actually-respond)
          (send/suspend/dispatch
           (lambda (embed-url)
             (let* ([url0 (request-uri (current-request))]
                    [url1 (string->url (embed-url actually-respond))])
               (make-redirect-response
                (make-url (url-scheme url1)
                          (url-user url1)
                          (url-host url1)
                          (url-port url1)
                          (url-path-absolute? url1)
                          (url-path url1)
                          (url-query url0)
                          (url-fragment url0))))))))
    
    ; expired-continuation-type -> xml
    (define/public (expired-continuation-xml type)
      (xml "You have been redirected from an expired web page. You should be able to proceed as normal. "
           "If you were in the process of making changes, please check to make sure they have been saved correctly."))
    
    ; -> void
    (define/public (on-full-response)
      (void))
    
    ; -> void
    (define/public (on-ajax-response)
      (void))
    
    ; [#:forward? boolean] -> (embed-url -> response)
    ;
    ; Makes a response-generator for use with send/suspend/dispatch. The response type varies 
    ; according to the type of request being handled:
    ;
    ;   - full page requests yield complete pages of XHTML;
    ;   - AJAX requests originating from event handlers in this page yield Javascript responses
    ;     that refresh appropriate parts of the page;
    ;   - AJAX requests originating from event handlers in other pages yield Javascript responses
    ;     that redirect the browser to this page (triggering a full page refresh).
    ;
    ; #:script allows the caller to specify a block of Javascript to run after the page has been 
    ; displayed or changed. This is useful for, for example, showing a message to the user or
    ; performing some update action. The script is executed after all other script execution and
    ; content rendering.
    (define/override (make-response-generator #:forward? [forward? #f])
      (if (ajax-request? (current-request))
          (if (and (not forward?) (equal? (ajax-request-page-id (current-request)) (get-component-id)))
              (make-ajax-response-generator)
              (make-ajax-redirect-response-generator))
          (if (post-request? (current-request))
              (make-full-redirect-response-generator)
              (make-full-response-generator))))
    
    ; -> (embed-url -> response)
    ;
    ; Makes a response-generator that creates a complete XHTML response for this page.
    (define/public (make-full-response-generator)
      (lambda (embed-url)
        (on-full-response)
        (log-frame-size)
        (parameterize ([javascript-rendering-mode (choose-rendering-mode dev?)])
          (log-response-time
           (smoke-response-types full)
           ; seed
           (define seed (make-seed this embed-url))
           (set-callback-codes! (make-callback-codes seed))
           ; Store the initial requirements for the page:
           (set-current-html-requirements! (delete-duplicates (get-html-requirements/fold)))
           (set-current-js-requirements! (delete-duplicates (get-js-requirements/fold)))
           ; Call render before get-on-attach for consistency with AJAX responses:
           (let ([code      (get-http-code)]
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
                               ,@(render-requirements (get-current-html-requirements) seed)
                               (script (@ [type "text/javascript"])
                                       (!raw "\n// <![CDATA[\n")
                                       (!raw ,(js ((function ($)
                                                     (!dot ($ document)
                                                           (ready (function ()
                                                                    (!dot Smoke (initialize ,(get-component-id)
                                                                                            ,(get-form-id)
                                                                                            (function ()
                                                                                              ; Init scripts:
                                                                                              ,@(render-requirements (get-current-js-requirements) seed)
                                                                                              ; Attach scripts:
                                                                                              ,(get-on-attach seed))))))))
                                                   jQuery)))
                                       (!raw "\n// ]]>\n")))
                         (body (@ ,@(core-html-attributes seed))
                               ,content)))))))))
    
    ; -> (embed-url -> response)
    ;
    ; Makes a response-generator that creates an AJAX Javascript response that 
    ; refreshes appropriate parts of this page.
    (define/public (make-ajax-response-generator)
      (lambda (embed-url)
        (on-ajax-response)
        (log-frame-size)
        (parameterize ([javascript-rendering-mode (choose-rendering-mode dev?)])
          (log-response-time
           (smoke-response-types ajax)
           ; seed
           (define seed (make-seed this embed-url))
           ; response
           (with-handlers ([exn? (lambda (exn)
                                   (display-exn exn)
                                   (make-js-response 
                                    #:code 500
                                    #:message "Internal Error"
                                    (js (!dot Smoke (log "An error has occurred. Talk to your system administrator.")))))])
             (let ([new-html-requirements (filter-new-requirements (get-current-html-requirements) (get-html-requirements/fold))]
                   [new-js-requirements   (filter-new-requirements (get-current-js-requirements)   (get-js-requirements/fold))])
               (unless (null? new-html-requirements)
                 (set-current-html-requirements! (append (get-current-html-requirements) new-html-requirements)))
               (unless (null? new-js-requirements)
                 (set-current-js-requirements! (append (get-current-js-requirements) new-js-requirements)))
               (make-js-response
                (js ((function ($)
                       ,(opt-js (not (null? new-html-requirements))
                          (!dot ($ (!dot Smoke documentHead))
                                (append ,(xml->string (xml ,@(render-requirements new-html-requirements seed))))))
                       ,@(render-requirements new-js-requirements seed)
                       ,@(map (cut send <> get-on-refresh seed)
                              (get-dirty-components)))
                     jQuery)))))))))
    
    ; -> (embed-url -> response)
    ;
    ; This response is sent as the first response from any page. It sets up
    ; a top web frame and makes sure that any AJAX operations the user performs
    ; aren't lost if they hit Reload.
    (define/public (make-full-redirect-response-generator)
      (lambda (embed-url)
        (parameterize ([javascript-rendering-mode (choose-rendering-mode dev?)])
          (log-response-time
           (smoke-response-types full-redirect)
           ; seed
           (define seed (make-seed this embed-url))
           (make-html-response
            #:code      301 
            #:message   "Moved Permanently"
            #:mime-type #"text/html"
            #:headers   (cons (make-header #"Location" (string->bytes/utf-8 (embed/thunk seed (cut respond))))
                              no-cache-http-headers)
            (xml))))))
    
    ; -> (embed-url -> response)
    ;
    ; Makes a response-generator that redirects the browser to this page.
    ;
    ; When this procedure is called, the current frame should be a child of
    ; the AJAX frame of the page. The rendering seed is set up to use the
    ; AJAX frame as the base frame for subsequent requests. The current frame
    ; is squeezed into the AJAX frame right before the response is sent.
    (define/public (make-ajax-redirect-response-generator)
      (lambda (embed-url)
        (parameterize ([javascript-rendering-mode (choose-rendering-mode dev?)])
          (log-response-time
           (smoke-response-types ajax-redirect)
           ; seed
           (define seed (make-seed this embed-url))
           ; response
           (make-js-response 
            (js (= (!dot window location)
                   ,(embed/thunk seed (cut respond)))))))))
    
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
                 ,(inner (xml "Page under construction.") render seed))
           (span (@ [id "smoke-ajax-spinner"] [class "ui-corner-all"])
                 "Loading...")))
    
    ; seed -> js
    (define/augment (get-on-attach seed)
      (js (!dot ($ ,(format "#~a" (get-form-id)))
                (bind "submit" (function (evt)
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
      (js (!dot ($ ,(format "#~a" (get-id)))
                (html ,(xml->string (render seed))))))
    
    ; seed -> js
    (define/augment (get-on-detach seed)
      (js (!dot ($ ,(format "#~a" (get-form-id))) (unbind))
          ,(inner (js) get-on-detach seed)))))

; Classes ----------------------------------------

(define html-page%
  (class/cells (html-page-mixin (page-mixin html-element%)) ()))

; Helpers ----------------------------------------

; (listof requirement) (listof requirement) -> (listof requirement)
(define (filter-new-requirements prev-reqs curr-reqs)
  (remove-duplicates
   (filter-map (lambda (req)
                 (and (not (memq req prev-reqs)) req))
               curr-reqs)))

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