#lang scheme/base

(require scheme/match
         scheme/pretty
         (planet untyped/unlib:3/list)
         (planet untyped/unlib:3/string)
         "../../lib-base.ss"
         "autocomplete-field.ss"
         "browser-util.ss"
         "text-input.ss"
         "combo-box.ss"
         "form-element.ss"
         "refreshable.ss")

; To use this class, override:
;   - get-item     : -> (list-of any) ; return a list of Scheme items to show in the list

; Vanilla Combo-box is basic elements; See below for more general combo-box%
(define vanilla-set-selector%
  (class/cells (refreshable-mixin form-element%) ()
    
    (inherit get-id
             get-classes
             get-enabled?
             core-html-attributes
             refresh!)
    
    ; Fields -------------------------------------
    ; string
    (init-field disabled-text "N/A" #:accessor #:mutator)
    ; string
    (init-field empty-text "(none)" #:accessor #:mutator)
    
    ; (cell (listof (alistof (U boolean symbol number) string)))
    (init-cell selected-items null #:accessor)
    (init-cell available-items null #:accessor)
    
    ; html-component%
    (init-field editor
      (new text-input%)
      #:child #:accessor #:mutator)
    
    ; Constructor --------------------------------
    
    (init [value #f])
    (init [classes null])    
    (super-new [classes (cons 'smoke-set-selector classes)])
    
    ; Public methods -----------------------------
    
    ; -> (listof (U xml (seed -> xml)))
    (define/augment (get-html-requirements)
      (list* rollover-script
             (inner null get-html-requirements)))
    
    ; -> (listof (U js (seed -> js)))
    (define/augment (get-js-requirements)
      (list* dismiss-script
             (inner null get-js-requirements)))
    
    ; -> any
    (define/override (get-value)
      #f)
    
    ; any -> void
    (define/override (set-value! value)
      #f)
    
    ; -> boolean
    (define/override (value-valid?)
      #t)
    
    ; -> boolean
    (define/override (value-changed?) #f)
    
    
    ; -> (listof (alistof (U boolean symbol number) string))
    (define/public-final (set-available-items! items)
      (web-cell-set! available-items-cell items)
      (refresh-selectable-items)) 
    
    ; -> (listof (alistof (U boolean symbol number) string))
    (define/public-final (set-selected-items! items)
      (web-cell-set! selected-items-cell items)
      (refresh-selectable-items))
    
    ; -> (listof (U boolean symbol number))
    (define/public-final (get-selected-values)
      (map car (get-selected-items)))
    
    ; (U boolean symbol number) -> void
    (define/public (select-item item) 
      ; selects an item, maintaining original sort order
      (let* ([full-item-data (assoc item (get-available-items))]
             [newly-selected (cons full-item-data (get-selected-items))]) 
        (set-selected-items! (filter (lambda (item)
                                       (member item newly-selected))
                                     (get-available-items))))
      (refresh-selectable-items))
    
    ; (U boolean symbol number) -> void
    (define/public (deselect-item item)
      (set-selected-items! (alist-delete item (get-selected-items)))
      (refresh-selectable-items)) 
    
    ; -> boolean
    (define/public (items-selectable?)
      (< (length (get-selected-items)) (length (get-available-items)))) 
    
    ; (listof (alist (U boolean symbol number) string))) -> void
    (define/public (set-selectable-items! items) 
      (error "set-selectable-items! must be overridden."))
    
    ; -> (U number symbol)
    (define/public (get-editor-value) 
      (error "get-editor-value must be overridden."))
    
    ; -> void
    (define/public (reset-editor-value) 
      (error "reset-editor-value must be overridden."))
    
    ; -> (listof (alist (U boolean symbol number) string)))
    (define/private (refresh-selectable-items)
      (let ([selected-items  (get-selected-items)]
            [available-items (get-available-items)])
        (set-selectable-items! (filter (lambda (item)
                                         (not (member item selected-items)))
                                       available-items))))
    
    
    ; seed -> xml
    (define/override (render seed)
      (let ([visible? (send (get-editor) get-visible?)])
        (xml 
         (div 
          (@ ,(core-html-attributes 
               seed 
               #:classes (cond 
                           [visible?             (cons 'edit-mode (get-classes))]
                           [(not (get-enabled?)) (cons 'disabled (get-classes))]
                           [else                 (get-classes)])))
          (ul (@ [class 'active])
              ,@(cond 
                  [(not (get-enabled?))
                   (list (xml (li ,(get-disabled-text))))]
                  [(null? (get-selected-items))
                   (list (xml (li (@ [class 'empty-text])
                                  ,(get-empty-text))))]
                  [else
                   (for/list ([item   (in-list (get-selected-items))]
                              [index  (in-naturals)])
                     (let* ([item-id (string->symbol 
                                      (format "~a-item~a" (get-id) index))]
                            [dismiss-id (string->symbol 
                                         (format "~a-dismiss" item-id))])
                       (xml 
                        (li (@ [id ,item-id])
                            ,(cdr item)
                            ,(opt-xml (get-enabled?)
                               (img (@ [id      ,dismiss-id] 
                                       [class   "rollover-img"]
                                       [src     "/images/smoke/dismiss.png"]
                                       [title   "Dismiss this item"]
                                       [alt     "Dismiss this item"]
                                       [onclick 
                                        ,(embed/ajax seed 
                                                     (callback dismiss-item (car item)))])))))))]))
          ,(opt-xml (and (debug* "enabled?" get-enabled?) (debug* "¬visible?" not visible?) (debug* "items-selectable?" items-selectable?)) 
             (div (@ [class 'item-entry])
                  (a (@ [id ,(string->symbol (format "~a-activate" (get-id)))] 
                        [onclick ,(embed/ajax seed (callback activate-item-entry))])
                     "Add item...")))
          ,(opt-xml (and (get-enabled?) visible?)
             (div (@ [class 'item-entry])
                  ,(send (get-editor) render seed)
                  (img (@ [class   "add-item rollover-img inner"]
                          [src     "/images/smoke/add.png"]
                          [title   "Add item"]
                          [alt     "Add item"]
                          [onclick ,(embed/ajax seed (callback activate-item))]))
                  (img (@ [id ,(string->symbol (format "~a-deactivate" (get-id)))] 
                          [class   "rollover-img"]
                          [src     "/images/smoke/dismiss.png"]
                          [title   "Deactivate editor"]
                          [alt     "Deactivate editor"]
                          [onclick ,(embed/ajax seed (callback deactivate-item-entry))])))))))) 
    
    ; (U boolean symbol number) -> void
    (define/public-final #:callback (dismiss-item item)
      (deselect-item item))
    
    ; -> void
    (define/public-final #:callback (activate-item-entry)
      (send (get-editor) set-visible?! #t)
      (refresh!))
    
    ; -> void
    (define/public-final #:callback (deactivate-item-entry)         
      (send (get-editor) set-visible?! #f)
      (refresh!))
    
    ; -> void
    (define/public-final #:callback (activate-item)      
      (let ([item (get-editor-value)])
        (when item
          (select-item item)
          (reset-editor-value) 
          (when (not (items-selectable?))
            (send (get-editor) set-visible?! #f)))))
    
    ; seed -> js
    (define/augride (get-on-change seed)
      (define id (get-id))
      (js (!dot Smoke (setSubmitData ,id (!dot Smoke (findById ,id) value)))))    
    
    ; seed -> js
    (define/augride (get-on-attach seed)
      ; (U symbol #f)
      (define id (get-id))
      (js ,@(for/list ([index (in-naturals)] [item (get-selected-items)])
              (define item-id (string->symbol (format "~a-item~a" id index)))
              (define dismiss-id (string->symbol (format "~a-dismiss" item-id)))
              (js (!dot Smoke SetSelector 
                        (initialize (!dot Smoke (findById ,item-id))
                                    (!dot Smoke (findById ,dismiss-id))))))
          (!dot (jQuery ,(format "#~a .rollover-img" (get-id))) (rollover "-hover"))))))


(define set-selector%
  (class/cells vanilla-set-selector% () 
    
    (inherit get-editor)
    
    ; Fields -------------------------------------        
    
    ; combo-box%
    (super-new [editor (new combo-box% [classes (list "editor")] [visible? #f])])
    
    ; Methods ------------------------------------
    ; -> void
    (define/override (set-selectable-items! items) 
      (send (get-editor) set-options! items))
    
    ; -> (U number symbol)
    (define/override (get-editor-value) 
      (send (get-editor) get-value))
    
    ; -> void
    (define/override (reset-editor-value) 
      (void)))) ; do nothing - combo default is fine


(define set-selector-autocomplete%
  (class/cells vanilla-set-selector% ()    
    
    (inherit get-editor set-editor! get-available-items get-id)
    
    ; Fields -------------------------------------
    
    ; autocomplete-field%    
    (super-new [editor (new autocomplete-field% 
                            [classes (list "editor")]
                            [visible? #f])])
    
    ; Methods ------------------------------------       
    ; (listof (alistof (U boolean symbol number) string)) -> void
    (define/override (set-selectable-items! items) 
      (send (get-editor) set-options! (map cdr items)))
    
    ; autocomplete-field% -> void
    (define/public (set-autocomplete-field! autocomplete)
      (set-editor! autocomplete))
    
    ; -> (U number symbol)
    (define/override (get-editor-value) 
      (let ([string-value (send (get-editor) get-value)])
        (for/or ([item (in-list (get-available-items))])
                (and (equal? string-value (cdr item)) (car item)))))
    
    ; -> void
    (define/override (reset-editor-value) 
      (send (get-editor) set-value! #f)))) ; clear autocomplete text


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

(provide vanilla-set-selector%
         set-selector%
         set-selector-autocomplete%)
