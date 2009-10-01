#lang scheme/base

(require (for-syntax scheme/base
                     "../../class/class.ss"
                     srfi/26
                     (planet untyped/unlib:3/syntax))
         "../../lib-base.ss"
         "html-component.ss")

; Interfaces -----------------------------------

; A component that is rendered as a single HTML element (and its contents).
(define html-element<%>
  (interface/events (html-component<%>)
    get-id                 ; -> symbol
    set-id!                ; symbol -> void
    get-classes            ; -> (listof symbol)
    set-classes!           ; (listof symbol) -> void
    get-style              ; -> string
    set-style!             ; string -> void
    get-tooltip            ; -> string
    set-tooltip!           ; string -> void
    core-html-attributes)) ; -> (alistof symbol (U string symbol number boolean (-> any)))

; Helpers ----------------------------------------

; (listof symbol)
; (listof symbol)
(define-values-for-syntax (event-names short-event-names)
  (let ([all-names '((load          load)
                     (unload        unload)
                     (focus         focus)
                     (blur          blur)
                     (click         click)
                     (double-click  dblclick)
                     (mouse-over    mouseover)
                     (mouse-out     mouseout)
                     (mouse-down    mousedown)
                     (mouse-up      mouseup)
                     (mouse-move    mousemove)
                     (key-press     keypress)
                     (key-down      keydown)
                     (key-up        keyup)
                     (abort         abort)
                     (error         error)
                     (select        select)
                     (change        change)
                     (reset         reset)
                     (submit        submit))])
    (values (map car  all-names)
            (map cadr all-names))))

; Syntax -----------------------------------------

; The macros below expand a mixin/events form into a mixin/cells form.
; The expand-foo procedures are used as fold accumulator functions: they
; accumulate a list of body clauses in reverse order.

; syntax -> syntax
(define-syntax (interface/events stx)
  (with-syntax ([(get-on-event ...) (map (cut make-id stx 'get-on- <>) event-names)])
    (syntax-case stx ()
      [(_ (super-interface ...)
          method ...)
       #'(interface (super-interface ...)
           method ... 
           get-on-event ...)])))

; syntax -> syntax
(define-syntax (mixin/events stx)
  (with-syntax ([get-visible?            (make-id stx 'get-visible?)]
                [(short-event ...)       (map (cut make-id stx <>) short-event-names)]
                [(event ...)             (map (cut make-id stx <>) event-names)]
                [(on-event ...)          (map (cut make-id stx 'on- <>) event-names)]
                [(get-on-event ...)      (map (cut make-id stx 'get-on- <>) event-names)]
                [(get-on-event/fold ...) (map (cut make-id stx 'get-on- <> '/fold) event-names)]
                [(set-on-event! ...)     (map (cut make-id stx 'set-on- <> '!) event-names)])
    
    ; syntax -> syntax
    (define (expand-clause clause-stx)
      (syntax-case* clause-stx (init-events) symbolic-identifier=?
        [(init-events get-id)
         #`(begin 
             
             ; Fields ----------------------------
             
             ; (cell (hashof symbol (listof (seed -> js))))
             ; Hash of event names ('click, 'change, etc...) to lists of javascript constructors.
             (field [default-event-handlers-cell (make-web-cell #hasheq())])
             
             ; Constructor -----------------------
             
             ; (U callback js (seed -> js))
             (init [on-event #f]) ...
             
             (when on-event (set-default-event-handler! 'event on-event)) ...
             
             ; Methods ---------------------------
             
             ; -> (hasheqof symbol (seed -> (U js #f)))
             (define/public (get-default-event-handlers)
               (web-cell-ref default-event-handlers-cell))
             
             ; (hasheqof symbol (seed -> (U js #f))) -> void
             (define/public (set-default-event-handlers! handlers)
               (web-cell-set! default-event-handlers-cell handlers))
             
             ; symbol -> (U (seed -> (U js #f)) #f)
             (define/public (get-default-event-handler name)
               (hash-ref (get-default-event-handlers) name #f))
             
             ; symbol (U callback js (seed -> js)) -> void
             (define/public (set-default-event-handler! name handler)
               (cond [(callback? handler)
                      (set-default-event-handler! name (cut embed/ajax <> handler))]
                     [(javascript? handler)
                      (set-default-event-handler! name (lambda (seed) handler))]
                     [(procedure? handler)
                      (set-default-event-handlers! (hash-set (get-default-event-handlers) name handler))]
                     [else (raise-type-error 'set-default-event-handler! 
                                             "(U callback js (seed -> js))"
                                             handler)]))
             
             ; (seed -> (U js #f))
             (define/pubment (get-on-event seed)
               (get-on-event/fold seed))
             
             ...
                          
             ; (seed -> (U js #f))
             (define/public (get-on-event/fold seed)
               ; (U js #f)
               (define inner-handler
                 (inner #f get-on-event seed))
               ; (U (seed -> (U js #f)) #f)
               (define make-default-handler
                 (get-default-event-handler 'event))
               ; (U js #f)
               (define default-handler 
                 (and make-default-handler (make-default-handler seed)))
               ; (U js #f)
               (if inner-handler
                   (if default-handler
                       (js:begin inner-handler default-handler)
                       inner-handler)
                   (if default-handler
                       default-handler
                       #f)))
             
             ...
             
             ; (U callback js (seed -> js)) -> void
             (define/public (set-on-event! handler)
               (set-default-event-handler! 'event handler))
             
             ...
             
             ; seed -> js
             ;
             ; This probably requires some explanation:
             ;
             ; get-on-attach in html-component calls get-on-attach/fold, which uses the inner version of get-on-attach
             ; for this component's script and a map over get-child-components for the childrens' attach script.
             ;
             ; Here we want to override the child-polling part of the behaviour from html-component, so instead of augmenting
             ; get-on-attach, we override get-on-attach/fold. We still want to do the default behaviour of get-on-attach/fold
             ; when the element is visible, though, and we have a call to super here to do that.
             (define/override (get-on-attach/fold seed)
               (opt-js (get-visible?)
                 ,(let ([handler (get-on-event seed)])
                    (opt-js handler
                      (!dot ($ ,(format "#~a" (get-id))) (bind 'short-event (function (evt) ,handler)))))
                 ...
                 ,(super get-on-attach/fold seed)))
             
             ; seed -> js
             ;
             ; See the comments for get-on-attach/fold above for an explanation of how this works.
             (define/override (get-on-detach/fold seed)
               (opt-js (get-visible?)
                 ,(super get-on-detach/fold seed)
                 (!dot ($ ,(format "#~a" (get-id))) (unbind)))))]
        [other #'other]))
    
    (syntax-case stx ()
      [(_ (src-interface ...) (des-interface ...) clause ...)
       #`(mixin/cells (src-interface ...) (des-interface ...)
           #,@(map expand-clause (syntax->list #'(clause ...))))])))

; Provide statements -----------------------------

(provide html-element<%>
         interface/events
         mixin/events)
