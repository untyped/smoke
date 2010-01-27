#lang web-server

(require srfi/13
         (planet untyped/unlib:3/list)
         "../../lib-base.ss"
         "browser-util.ss"
         "text-input.ss")

(define autocomplete-field%
  (class/cells text-input% ()
    
    (inherit get-id
             get-raw
             set-raw!
             get-value
             get-enabled?
             core-html-attributes)
    
    ; Fields -------------------------------------
    
    ; (cell (U natural #f))
    (init-cell size #f #:accessor #:mutator)
    
    ; (cell (U natural #f))
    (init-cell max-length #f #:accessor #:mutator)
    
    ; (cell (listof option))
    ;
    ; where option: 
    ;   string          if multi-column? is #f
    ;   (listof string) if multi-column? is #t
    (init-cell options null #:mutator)
    
    ; (cell integer)
    (init-cell min-trigger-length 3 #:accessor #:mutator)
    
    ; (cell boolean)
    (init-cell multi-column? #f #:accessor #:mutator)
    
    ; Constructor --------------------------------
    
    ; (listof symbol)
    (init [classes null])
    
    (super-new [classes (cons 'smoke-autocomplete-field classes)])
    
    ; Public methods -----------------------------
    
    ; -> (listof (U html (seed -> html)))
    (define/augment (get-html-requirements)
      (list* autocomplete-script
             (inner null get-html-requirements)))
    
    ; -> (listof (U symbol string)
    (define/override (get-classes)
      (if (zero? (get-min-trigger-length))
          (list* "smoke-autocomplete-field-trigger" (super get-classes))
          (super get-classes)))
    
    ; seed -> xml
    (define/override (render seed)
      (define id         (get-id))
      (define size       (get-size))
      (define max-length (get-max-length))
      (define raw        (get-raw))
      (xml (input (@ ,(core-html-attributes seed)
                     [type "text"]
                     ,(opt-xml-attr size)
                     ,(opt-xml-attr max-length maxlength max-length)
                     [value ,raw]))))
    
    ; request -> void
    (define/augride (on-request request)
      (when (get-enabled?)
        (let ([binding    (request-binding-ref request (get-id))]
              [max-length (get-max-length)])
          (when binding 
            (set-raw! (if (and max-length (< max-length (string-length binding)))
                          (substring binding 0 max-length)
                          binding))))))
    
    ; -> void
    (define/public #:callback (on-popup)
      (define prefix  (request-binding-ref (current-request) 'prefix))
      (define options (get-options prefix))
      (send/back (make-js-response 
                  (js ((function ()
                         ; autocomplete.js is written as if all autocompletes are
                         ; multi-column, so return an arrayOf(arrayOf(string)):
                         (return ,(apply js:array 
                                         (map (if (get-multi-column?)
                                                  (cut apply js:array <>)
                                                  js:array)
                                              options)))))))))
    
    ; string (U string #f) -> boolean
    (define (option+column-matches? option prefix)
      (or (not prefix) 
          (let ([index (string-contains option prefix)])
            (and index (zero? index)))))
    
    ; string -> (listof string)
    (define/public (get-options prefix)
      ; option -> boolean
      ; where option: (U string (listof string))
      (define option-matches?
        (if (get-multi-column?)
            (lambda (option)
              (ormap (lambda (column)
                       (option+column-matches? column prefix))
                     option))
            (lambda (option)
              (option+column-matches? option prefix))))
      ; (listof string)
      (filter option-matches? (web-cell-ref options-cell)))
    
    ; seed -> js
    (define/augride (get-on-focus seed)
      (js (!dot Smoke Autocomplete (onFocus this evt))))
    
    ; seed -> js
    (define/augride (get-on-blur seed)
      (js (!dot Smoke Autocomplete (onBlur this evt))))
    
    ; seed -> js
    (define/augride (get-on-click seed)
      (js (!dot Smoke Autocomplete
                (onClick this evt ,(embed/full seed (callback on-popup)) ,(get-min-trigger-length)))))
    
    ; seed -> js
    (define/augride (get-on-key-down seed)
      (js (!dot Smoke Autocomplete
                (onKeyDown this evt ,(embed/full seed (callback on-popup)) ,(get-min-trigger-length)))))
    
    ; seed -> js
    (define/augride (get-on-attach seed)
      (js (!dot ($ ,(format "#~a" (get-id))) 
                (attr "autocomplete" "off"))))))

; Helpers ----------------------------------------

; xml
(define autocomplete-script
  (xml (script (@ [type "text/javascript"]
                  [src  "/scripts/smoke/autocomplete.js"]))))

; Provide statements -----------------------------

(provide autocomplete-field%)
