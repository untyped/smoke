#lang scheme/base

(require scheme/match
         scheme/pretty
         ;(planet untyped/unlib:3/list)
         ;(planet untyped/unlib:3/string)
         "../../lib-base.ss"
         "browser-util.ss"
         "form-element.ss"
         "refreshable.ss")

; Interfaces -------------------------------------

(define set-selector<%>
  (interface (form-element<%>)
    get-editor-value    ; -> any
    reset-editor-value  ; -> void
    item->raw           ; any -> (U boolean integer symbol)
    raw->item           ; (U boolean integer symbol) -> any
    item->string        ; any -> string
    get-available-items ; -> (listof any)
    select-item         ; any -> void
    deselect-item       ; any -> void
    items-available?))  ; -> boolean

; Generic classes --------------------------------

(define vanilla-set-selector%
  (class/cells (refreshable-mixin form-element%) (set-selector<%>)
    
    (inherit get-id
             get-classes
             get-enabled?
             core-html-attributes
             refresh!)
    
    ; Fields -------------------------------------
    ; string
    (init-field disabled-text "N/A" #:accessor)
    ; string
    (init-field empty-text "(none)" #:accessor)
    
    ; (cellof (listof any))
    (init-cell value null #:override-accessor)
    ; boolean
    (init-cell has-value-changed? #f #:accessor #:mutator)
    
    ; Editors ------------------------------------
    
    ; html-component%
    (init-field editor (error "editor must be set")
                #:child #:accessor #:mutator)
    
    ; Constructor --------------------------------
    
    ; (listof (U symbol string))
    (init [classes null])
    
    (super-new [classes (cons 'smoke-set-selector classes)])
    
    ; Public methods -----------------------------
    
    ; -> (listof (U xml (seed -> xml)))
    (define/augment (get-html-requirements)
      (list* rollover-script (inner null get-html-requirements)))
    
    ; -> (listof (U js (seed -> js)))
    (define/augment (get-js-requirements)
      (list* dismiss-script (inner null get-js-requirements)))
    
    ; (listof any) -> void
    (define/override (set-value! value)
      (unless (equal? value (get-value))
        (set-has-value-changed?! #t))
      (web-cell-set! value-cell value)
      (refresh-selectable-items))
    
    ; -> boolean
    (define/override (value-valid?)
      #t) ; values are always valid, since the list must only contain valid values
    
    ; -> boolean
    (define/override (value-changed?)
      (begin0 (get-has-value-changed?)
              (set-has-value-changed?! #f)))
    
    ; Interface methods --------------------------
    
    ; -> (U number symbol)
    (define/public (get-editor-value) 
      (error "get-editor-value must be overridden."))
    
    ; -> void
    (define/public (reset-editor-value) 
      (error "reset-editor-value must be overridden."))
    
    ; any -> (U boolean integer symbol)
    (define/public (item->raw item)
      (error "item->raw must be overridden"))
    
    ; (U boolean integer symbol) -> any
    (define/public (raw->item raw)
      (error "raw->item must be overridden"))
    
    ; any -> string
    (define/public (item->string item)
      (error "item->string must be overridden"))
    
    ; -> (listof any)
    (define/public (get-available-items)
      (error "get-available-items must be overridden"))
    
    ; Adds item to the list of selected items, maintaining original sort order
    ; any -> void
    (define/public (select-item item) 
      (let ([unordered-value (cons item (get-value))]) 
        (set-value! (filter (cut member <> unordered-value) (get-available-items))))
      (refresh-selectable-items))
    
    ; any -> void
    (define/public (deselect-item item)
      (set-value! (remove item (get-value)))
      (refresh-selectable-items))
    
    ; -> boolean
    (define/public (items-available?)
      (error "items-available? must be overridden"))
    
    ; -> void
    (define/public (refresh-selectable-items)
      (error "refresh-selectable-items must be overridden"))
    
    ; Rendering ----------------------------------
    
    ; seed -> xml
    (define/override (render seed)
      (let ([visible? (send (get-editor) get-visible?)])
        (xml 
         (div (@ ,(core-html-attributes seed #:classes (cond [visible?             (cons 'edit-mode (get-classes))]
                                                             [(not (get-enabled?)) (cons 'disabled (get-classes))]
                                                             [else                 (get-classes)])))
              (ul (@ [class 'active])
                  ,@(cond [(not (get-enabled?))
                           (list (xml (li ,(get-disabled-text))))]
                          [(null? (get-value))
                           (list (xml (li (@ [class 'empty-text]) ,(get-empty-text))))]
                          [else
                           (for/list ([item   (in-list (get-value))]
                                      [index  (in-naturals)])
                             (let* ([item-id    (string->symbol (format "~a-item~a" (get-id) index))]
                                    [item-raw   (item->raw item)]
                                    [item-str   (item->string item)]
                                    [dismiss-id (string->symbol (format "~a-dismiss" item-id))])
                               (xml 
                                (li (@ [id ,item-id])
                                    ,item-str
                                    ,(opt-xml (get-enabled?)
                                       (img (@ [id      ,dismiss-id] 
                                               [class   "rollover-img"]
                                               [src     "/images/smoke/dismiss.png"]
                                               [title   "Dismiss this item"]
                                               [alt     "Dismiss this item"]
                                               [onclick ,(embed/ajax seed (callback dismiss-item item-raw))])))))))]))
              ,(cond [(and (get-enabled?) visible?)
                      (xml (div (@ [class 'item-entry])
                                ,(send (get-editor) render seed)
                                (img (@ [class   "add-item rollover-img inner"]
                                        [src     "/images/smoke/add.png"]
                                        [title   "Add item"]
                                        [alt     "Add item"]
                                        [onclick ,(embed/ajax seed (callback activate-item))]))))]
                     [else (xml)])))))
    
    ; Callbacks ----------------------------------
    
    ; (U boolean symbol number) -> void
    (define/public-final #:callback (dismiss-item item)
      (deselect-item (raw->item item))
      (refresh-editor))
    
    ; -> void
    (define/public-final #:callback (activate-item)      
      (let ([item (and (get-editor-value) (raw->item (get-editor-value)))])
        (when item 
          (select-item item)
          (reset-editor-value) 
          (refresh-editor))))
    
    ; -> void
    (define/public-final (refresh-editor)
      (send (get-editor) set-visible?! (items-available?)))
    
    ; seed -> js
    (define/augride (get-on-change seed)
      (define id (get-id))
      (js (!dot Smoke (setSubmitData ,id (!dot Smoke (findById ,id) value)))))    
    
    ; seed -> js
    (define/augride (get-on-attach seed)
      ; (U symbol #f)
      (define id (get-id))
      (js ,@(for/list ([index (in-naturals)]
                       [item  (get-value)])
              (define item-id    (string->symbol (format "~a-item~a" id index)))
              (define dismiss-id (string->symbol (format "~a-dismiss" item-id)))
              (js (!dot Smoke SetSelector 
                        (initialize (!dot Smoke (findById ,item-id))
                                    (!dot Smoke (findById ,dismiss-id))))))
          (!dot (jQuery ,(format "#~a .rollover-img" (get-id))) (rollover "-hover"))))))

; Helpers ----------------------------------------

; js
(define dismiss-script
  (js (= (!dot Smoke SetSelector)
         (!object))
      ; element element -> void
      (= (!dot Smoke SetSelector initialize)
         (function (notification dismiss)
           (!dot ($ dismiss)
                 (click (function ()
                          (!dot ($ dismiss)
                                (unbind))
                          (!dot ($ notification)
                                (unbind)
                                (fadeOut "fast")))))))))

; Provide statements -----------------------------

(provide set-selector<%>
         vanilla-set-selector%)