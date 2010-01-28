#lang scheme

(require "../../lib-base.ss"
         "../component.ss")

; Structure types --------------------------------

; (struct (any ... -> void) boolean)
(define-struct callback-metadata (procedure respond?) #:transparent)

; Interfaces -------------------------------------

(define html-component<%>
  (interface (component<%>)
    get-html-requirements      ; -> (listof (U xml (seed -> xml)))
    get-js-requirements        ; -> (listof (U js (seed -> js)))
    get-html-requirements/fold ; -> (listof (U xml (seed -> xml)))
    get-js-requirements/fold   ; -> (listof (U js (seed -> js)))
    get-on-refresh             ; seed -> js
    get-on-attach              ; seed -> js
    get-on-render              ; seed -> js
    get-on-detach))            ; seed -> js

; Classes ----------------------------------------

(define html-component-mixin
  (mixin/cells (component<%>) (html-component<%>)
    
    (inherit get-component-id
             get-child-components
             get-all-components)
    
    ; Fields -------------------------------------
    
    ; (hasheqof symbol callback-metadata)
    (field callback-metadata-cache (make-hasheq))
    
    ; Rendering and scripts ----------------------
    
    ; seed -> xml
    ;
    ; Returns the XML content for this component and its subtree.
    (define/override (render seed)
      (xml ,@(map (cut send <> render seed)
                  (get-child-components))))
    
    ; -> (listof (U xml (seed -> xml)))
    ;
    ; Returns a list of <head> tags that are required for objects of this class to 
    ; work properly. New tags discovered duing an AJAX response are added and evaluated
    ; using JavaScript.
    (define/pubment (get-html-requirements)
      (inner null get-html-requirements))
    
    ; -> (listof (U xml (seed -> xml)))
    (define/public (get-html-requirements/fold)
      (append (get-html-requirements)
              (append-map (cut send <> get-html-requirements/fold)
                          (get-child-components))))
    
    ; -> (listof (U js (seed -> js)))
    ;
    ; Returns a list of Javascript blocks (and constructors for blocks) that are required
    ; for objects of this class to work properly. New fragments are added to the
    ; DOM-ready handler when the initial page is being rendered, and the beginning
    ; of the Javascript payload when AJAX responses are being rendered.
    (define/pubment (get-js-requirements)
      (inner null get-js-requirements))
    
    ; -> (listof (U js (seed -> js)))
    (define/public (get-js-requirements/fold)
      (append (append-map (cut send <> get-js-requirements/fold)
                          (get-child-components))
              (get-js-requirements)))
    
    ; seed -> js
    ;
    ; Returns a block of Javascript to refresh this component and its entire subtree.
    ;
    ; The default implementation collects:
    ;   - detach scripts from all subtree components *in the previous web frame*;
    ;   - a render script that refreshes the XML content of the subtree;
    ;   - attach scripts from all subtree components *in the current web frame*.
    (define/public (get-on-refresh seed)
      (define id (send this get-component-id))
      (js (try ,(with-old-web-frame (get-on-detach seed))
               (catch exn (!dot Smoke (badDetach exn))))
          (try ,(get-on-render seed)
               (catch exn (!dot Smoke (badRender exn))))
          (try ,(get-on-attach seed)
               (catch exn (!dot Smoke (badAttach exn))))))
    
    ; seed -> js
    ;
    ; Returns a block of Javascript to run after this component is refreshed.
    ; Augment this method to include additional attach functionality.
    (define/pubment (get-on-attach seed)
      (get-on-attach/fold seed))
    
    ; seed -> js
    ;
    ; This default implementation of get-on-attach traverses the subtree and collects
    ; JS fragments from all subcomponents.
    (define/public (get-on-attach/fold seed)
      (js ,(inner (js) get-on-attach seed)
          ,@(map (cut send <> get-on-attach seed)
                 (get-child-components))))
    
    ; seed -> js
    ;
    ; Returns a block of Javascript to render the DOM content of this component and its subtree.
    ;
    ; The default implementation does nothing.
    (define/public (get-on-render seed)
      (js))
    
    ; seed -> js
    ;
    ; Returns a block of Javascript to run before this component is refreshed.
    ;
    ; The default implementation traverses the subtree *in the previous web frame* and collects
    ; JS fragments from all subcomponents. Individual components should override this method 
    ; and include a supercall to ensure the entire tree is visited.
    (define/pubment (get-on-detach seed)
      (get-on-detach/fold seed))
    
    ; seed -> js
    ;
    ; This default implementation of get-on-attach traverses the subtree and collects
    ; JS fragments from all subcomponents.
    (define/public (get-on-detach/fold seed)
      (js ,@(map (cut send <> get-on-detach seed)
                 (get-child-components))
          ,(inner (js) get-on-detach seed)))
    
    ; Callbacks ----------------------------------
    
    ; symbol -> callback-metadata
    (define/public (get-callback-metadata id)
      (hash-ref callback-metadata-cache id
                (cut error (format "~a: no such callback: ~a"
                                   (class-name (object-class this))
                                   id))))
    
    ; symbol -> symbol
    (define/public (verify-callback-id id)
      ; Can't verify the ID if we're still constructing the object:
      (if (hash? callback-metadata-cache)
          (and (get-callback-metadata id) id)
          id))
    
    ; symbol list -> any
    (define/public (call-callback id args)
      ; callback-metadata
      (define meta (get-callback-metadata id))
      (if (callback-metadata-respond? meta)
          (begin (apply (callback-metadata-procedure meta) args)
                 (send (current-page) respond))
          (begin (apply (callback-metadata-procedure meta) args))))
    
    ; symbol (any ... -> void) boolean -> void
    (define/public (register-callback! id proc respond?)
      (hash-set! callback-metadata-cache id (make-callback-metadata proc respond?)))))

(define html-component%
  (class/cells (html-component-mixin component%) ()))

; Provide statements -----------------------------

(provide html-component<%>
         html-component-mixin
         html-component%)
