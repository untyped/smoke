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
             get-visible?
             core-html-attributes
             refresh!)
    
    ; Fields -------------------------------------
    ; string
    (init-field disabled-text "N/A" #:accessor)
    ; string
    (init-field empty-text "(none)" #:accessor)
    
    ; (cellof (listof any))
    (init-cell value null #:override-accessor)
    
    ; Editors ------------------------------------
    
    ; html-component%
    (init-field editor (error "editor must be set")
      #:child #:accessor #:mutator)
    
    ; Constructor --------------------------------
    
    ; (listof (U symbol string))
    (init [classes null])
    
    (super-new [classes (list* 'smoke-set-selector 'ui-widget 'ui-corner-all classes)])
    
    ; Public methods -----------------------------
    
    ; (listof any) -> void
    (define/override (set-value! value)
      (web-cell-set! value-cell value)
      (refresh-selectable-items))
    
    ; -> boolean
    (define/override (value-valid?)
      #t) ; values are always valid, since the list must only contain valid values
        
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
      (set-value! (filter (cute member <> (cons item (get-value)))
                          (get-available-items)))
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
      (let* ([id       (get-id)]
             [visible? (send (get-editor) get-visible?)]
             [classes  (if (get-enabled?)
                           (get-classes)
                           (cons 'disabled (get-classes)))])
        (xml (div (@ ,(core-html-attributes seed #:classes classes))
                  ,(opt-xml (pair? (get-value))
                     (ul (@ [class 'active])
                         ,@(for/list ([item   (in-list (get-value))]
                                      [index  (in-naturals)])
                             (let* ([item-id    (format "~a-item~a" id index)]
                                    [dismiss-id (format "~a-item~a-dismiss" id index)]
                                    [item-raw   (item->raw item)]
                                    [item-str   (item->string item)])
                               (xml (li (@ [id ,item-id])
                                        ,item-str
                                        ,(opt-xml (get-enabled?)
                                           (span (@ [id    ,dismiss-id] 
                                                    [class "remove-button ui-icon ui-icon-close ui-state-default"]
                                                    [title "Remove this item"]
                                                    [alt   "Remove this item"])))))))))
                  ,(opt-xml (and (get-enabled?) visible? (items-available?))
                     (div (@ [class "item-entry"])
                          ,(send (get-editor) render seed)
                          (span (@ [class "add-button ui-icon ui-icon-plus ui-state-default"]
                                   [title "Add item"]
                                   [alt   "Add item"]))))))))
    
    ; Callbacks ----------------------------------
    
    ; (U boolean symbol number) -> void
    (define/public-final #:callback (dismiss-item item)
      (when (and (get-enabled?) (get-visible?))
        (deselect-item (raw->item item))))
    
    ; -> void
    (define/public-final #:callback (activate-item)
      (when (and (get-enabled?) (get-visible?))
        (let ([item (and (get-editor-value) (raw->item (get-editor-value)))])
          (when item 
            (select-item item)
            (reset-editor-value)))))
    
    ; seed -> js
    (define/augride (get-on-attach seed)
      (let ([id (get-id)])
        (opt-js (and (get-enabled?) (get-visible?))
          ,@(for/list ([index (in-naturals)]
                       [item  (get-value)])
              (js (var [item    ($ ,(format "#~a-item~a" id index))]
                       [dismiss ($ ,(format "#~a-item~a-dismiss" id index))])
                  (!dot ($ dismiss)
                        (click (function ()
                                 ,(embed/ajax seed (callback dismiss-item (item->raw item)))
                                 (!dot ($ dismiss)
                                       (unbind))
                                 (!dot ($ item)
                                       (unbind)
                                       (fadeOut "fast"))))
                        (hover (function () (!dot ($ this) (addClass    "ui-state-highlight")))
                               (function () (!dot ($ this) (removeClass "ui-state-highlight")))))))
          ,(opt-js (items-available?)
             (!dot ($ ,(format "#~a .add-button" id))
                   (click (function ()
                            ,(embed/ajax seed (callback activate-item))))
                   (hover (function () (!dot ($ this) (addClass    "ui-state-highlight")))
                          (function () (!dot ($ this) (removeClass "ui-state-highlight")))))))))
    
    ; seed -> js
    (define/augride (get-on-detach seed)
      (let ([id (get-id)])
        (opt-js (and (get-enabled?) (get-visible?))
          ,@(for/list ([index (in-naturals)]
                       [item  (get-value)])
              (js (!dot ($ ,(format "#~a-item~a-dismiss" id index))
                        (unbind))))
          ,(opt-js (items-available?)
             (!dot ($ ,(format "#~a .add-button" id)) (unbind))))))))

; Provide statements -----------------------------

(provide set-selector<%>
         vanilla-set-selector%)