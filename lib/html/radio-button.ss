#lang web-server

(require scheme/match
         (only-in srfi/1/list delete)
         srfi/26/cut
         (planet untyped/unlib:3/string)
         (planet untyped/unlib:3/symbol)
         "../../lib-base.ss"
         "form-element.ss"
         "disableable-element.ss"
         "labelled-element.ss")

; Both radio-group% and all radio-button%s must be added as children of
; the component in which they are rendered.

(define radio-group%
  (class/cells form-element% ()
    
    (inherit get-id
             get-enabled?
             get-child-components)
    
    ; Fields -------------------------------------
    
    ; (cell (U radio-button% #f))
    (cell selected #f #:accessor)
    
    ; (cell (list-of radio-button%))
    (cell buttons null #:accessor #:mutator)
    
    ; Constructor ------------------------------
    
    ; any
    (init [selected #f])
    
    (when selected (set-selected! selected))
    
    (super-new)
    
    ; Public methods -----------------------------
    
    ; -> any
    (define/override (get-value)
      (and (get-selected)
           (send (get-selected) get-value)))
    
    ; any -> void
    (define/override (set-value! value)
      (set-selected! (ormap (lambda (button)
                              (and (equal? (send button get-value) value) button))
                            (get-buttons))))
    
    ; radio-button% -> void
    (define/public (set-selected! button)
      (if (or (not button) (is-a? button radio-button%))
          (web-cell-set! selected-cell button)
          (raise-exn exn:fail:contract
                     (format "Expected (U radio-button% #f), received ~s" button))))
    
    ; symbol -> void
    (define/public (set-selected/id! id)
      (set-selected! (ormap (lambda (button)
                              (and (equal? (send button get-id) id) button))
                            (get-buttons))))
    
    ; -> boolean
    (define/override (value-changed?)
      (web-cell-changed? selected-cell))
    
    ; seed -> xml
    (define/override (render seed)
      (xml))
    
    ; request -> void
    (define/augment (on-request request)
      (when (get-enabled?)
        (let ([binding (request-binding-ref request (get-id))])
          (when binding (set-selected/id! (string->symbol binding))))))))

(define radio-button%
  (class/cells (labelled-element-mixin disableable-element%) ()
    
    (inherit get-id
             core-html-attributes
             render-label)
    
    ; Fields -----------------------------------
    
    ; (cell radio-group%)
    (cell group #f #:accessor)
    
    ; (cell any)
    ; Given a gensym by default, to make explicit the link between 
    ; radio-group:selected and radio-group:value. This link is created
    ; by equality-testing the value of the selected item, hence we need
    ; distinct values by default.
    (init-cell value (gensym/interned 'radio-button) #:accessor #:mutator)
    
    ; Constructor ------------------------------
    
    ; button-group<%>
    (init [group #f])
    
    ; (listof symbol)
    (init [classes null])
    
    (when group (set-group! group))
    (super-new [classes (list* 'smoke-radio-button 'ui-widget classes)])
    
    ; Public methods ---------------------------
    
    ; radio-group% -> void
    (define/public (set-group! new-group)
      (define old-group (get-group))
      (when old-group (send old-group set-buttons! (delete this (send old-group get-buttons))))
      (web-cell-set! group-cell new-group)
      (when new-group (send new-group set-buttons! (cons this (send new-group get-buttons)))))
    
    ; -> boolean
    (define/override (get-enabled?)
      (and (super get-enabled?) 
           (or (and (get-group) (send (get-group) get-enabled?))
               (and (not (get-group)) #t))))
    
    ; seed -> xml
    (define/override (render seed)
      (define id       (get-id))
      (define group    (get-group))
      (define name     (send group get-id))
      (define value    (get-id))
      (define checked? (equal? (get-value) (send group get-value)))
      (define enabled? (get-enabled?))
      (xml (span (@ ,@(core-html-attributes seed #:id (symbol-append id '-wrapper)))
                 (input (@ ,@(core-html-attributes seed)
                           [type  "radio"]
                           [name  ,name]
                           [value ,value]
                           ,@(if checked? (xml-attrs [checked "checked"]) null)
                           ,@(if enabled? null (xml-attrs [disabled "disabled"]))))
                 ,(render-label seed))))
    
    ; seed -> js
    (define/override (get-on-render seed)
      (js (!dot Smoke (insertHTML (!dot Smoke (findById ,(symbol-append (get-id) '-wrapper)))
                                  "replace"
                                  ,(xml->string (render seed))))))
    
    ; seed -> js
    (define/augment (get-on-click seed)
      (define id (send (get-group) get-id))
      (define value (get-id))
      (js (!dot Smoke (setSubmitData ,id ,value))
          ,(inner (js) get-on-click seed)))))

; Provide statements -----------------------------

(provide radio-group%
         radio-button%)
