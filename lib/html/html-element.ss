#lang scheme/base

(require srfi/13
         (planet untyped/unlib:3/for)
         "../../lib-base.ss"
         "html-component.ss"
         "html-element-internal.ss")

; Mixins ---------------------------------------

(define html-element-mixin
  (mixin/events (html-component<%>) (html-element<%>)
    
    (inherit get-component-id
             get-on-detach
             get-on-attach)
    
    ; Fields -------------------------------------
    
    (super-new)
    
    ; (cell (U symbol #f))
    (cell id (get-component-id) #:accessor #:mutator)
    
    ; (cell (listof symbol))
    (init-cell classes null #:accessor #:mutator)
    
    ; (cell (U string #f))
    (init-cell style #f #:accessor #:mutator)
    
    ; (cell (U string #f))
    (init-cell tooltip #f #:accessor #:mutator)
    
    ; (cell boolean)
    (init-cell visible? #t #:accessor #:mutator)
    
    ; Constructor --------------------------------
    
    (init [id (get-component-id)])
    (set-id! id)
    
    ; Methods ------------------------------------
    
    (init-events get-id)
    
    ; seed
    ; [#:id      (U symbol #f)]
    ; [#:classes (listof (U string symbol))]
    ; [#:style   (U string #f)] 
    ; [#:tooltip (U string #f)]
    ; ->
    ; (listof attribute)
    (define/public (core-html-attributes
                    seed
                    #:id      [id      (get-id)]
                    #:classes [classes (get-classes)]
                    #:style   [style   (get-style)]
                    #:tooltip [title   (get-tooltip)])
      (xml-attrs ,@(opt-xml-attr id)
                 ,@(opt-xml-attr (pair? classes) class (format-classes classes))
                 ,@(opt-xml-attr style)
                 ,@(opt-xml-attr title)))
    
    ; seed -> xml
    (define/overment (render seed)
      (render/fold seed))
    
    ; seed -> xml
    (define/public (render/fold seed)
      (if (get-visible?)
          (inner (xml) render seed)
          (xml (span (@ [id ,(get-id)] [class 'ui-helper-hidden])))))
    
    ; seed -> js
    ;
    ; Returns a block of Javascript to refresh this component and its entire subtree.
    ;
    ; The default implementation collects:
    ;   - detach scripts from all subtree components *in the previous web frame*;
    ;   - a render script that refreshes the XML content of the subtree;
    ;   - attach scripts from all subtree components *in the current web frame*.
    (define/override (get-on-refresh seed)
      (let ([id (get-id)])
        (js (try ,(call-with-frame (frame-parent (current-frame))
                    (cut get-on-detach seed))
                 (catch exn (!dot Smoke (badDetach exn ,id))))
            (try ,(get-on-render seed)
                 (catch exn (!dot Smoke (badRender exn ,id))))
            (try ,(get-on-attach seed)
                 (catch exn (!dot Smoke (badAttach exn ,id)))))))
    
    ; seed -> js
    (define/override (get-on-render seed)
      (let ([id (get-id)])
        (js (!dot Smoke (insertHTML (!dot Smoke (findById ,id))
                                    "replace"
                                    ,(xml->string (render seed)))))))
    
    ; Printing -----------------------------------
    
    ; output-port (any output-port -> void) (U symbol #f) -> void
    (define/override (custom-print out print class-name)
      (print (vector (or class-name 'unknown-html-element)
                     (with-handlers ([exn? (lambda (exn) '<no-component-id>)])
                       (get-component-id))
                     (with-handlers ([exn? (lambda (exn) '<no-id>)])
                       (get-id)))
             out))))

(define html-element%
  (class/cells (html-element-mixin html-component%) ()
    ; seed -> xml
    (define/augride (render seed)
      (xml))))

; Procedures -------------------------------------

; (U string symbol) -> boolean
(define (html-id? str+sym)
  (let ([str (if (string? str+sym) str+sym (symbol->string str+sym))])
    (and (regexp-match #rx"^[a-zA-Z][a-zA-Z0-9_:.-]*$" str) #t)))

; (U string symbol) -> symbol
(define (html-id-encode str+sym)
  (let* ([str     (if (string? str+sym) str+sym (symbol->string str+sym))]
         [encoded (for/fold/reverse
                   ([accum null])
                   ([char (in-string str)])
                   (if (or (char-alphabetic? char)
                           (char-numeric?    char)
                           (memq char (list #\- #\_ #\: #\.)))
                       (cons char accum)
                       `(,@(reverse (string->list (string-upcase (number->string (char->integer char) 16)))) #\_ ,@accum)))])
    (string->symbol
     (apply string (if (char-alphabetic? (car encoded))
                       encoded
                       (list* #\i #\d encoded))))))

; Helpers ----------------------------------------

; (listof (U symbol string)) -> string
(define (format-classes classes)
  (string-join (map (lambda (class)
                      (if (string? class)
                          class
                          (symbol->string class)))
                    classes)
               " "))

; Provide statements -----------------------------

(provide html-element<%>
         html-element-mixin
         html-element%)

(provide/contract
 [html-id?       (-> (or/c string? symbol?) boolean?)]
 [html-id-encode (-> (or/c string? symbol?) symbol?)])
