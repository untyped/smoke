#lang scheme/base

(require srfi/13/string
         "../../lib-base.ss"
         "html-component.ss"
         "html-element-internal.ss")

; Mixins ---------------------------------------

(define html-element-mixin
  (mixin/events (html-component<%>) (html-element<%>)
    
    (inherit get-component-id)
    
    ; Fields -------------------------------------
    
    (super-new)
    
    ; (cell (U symbol #f))
    (cell [id (get-component-id)]
      #:accessor #:mutator)
    
    ; (cell (listof symbol))
    (init-cell [classes null]
      #:accessor #:mutator)
    
    ; (cell (U string #f))
    (init-cell [style #f] 
      #:accessor #:mutator)
    
    ; (cell (U string #f))
    (init-cell [tooltip #f]
      #:accessor #:mutator)
    
    ; (cell boolean)
    (init-cell [visible? #t] 
      #:accessor #:mutator)
    
    ; Constructor --------------------------------
    
    (init [id (get-component-id)])
    (set-id! id)
    
    ; Methods ------------------------------------
    
    (init-events get-id)
      
    ; seed
    ; [#:id      (U symbol #f)]
    ; [#:classes (listof (U string symbol))]
    ; [#:style   (U string #f)] 
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
          (xml (span (@ [id ,(get-id)] [class 'smoke-hidden-component])))))
    
    ; seed -> js
    (define/override (get-on-render seed)
      (js (!dot Smoke (insertHTML (!dot Smoke (findById ,(get-id)))
                                  "replace"
                                  ,(xml->string (render seed))))))))

(define html-element%
  (class/cells (html-element-mixin html-component%) ()
    ; seed -> xml
    (define/augride (render seed)
      (xml))))

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
