#lang web-server

(require "../../lib-base.ss")

(require "combo-box.ss"
         "set-selector.ss")

; A set-selector comprising an (alistof (U boolean integer symbol) . string)

(define set-selector-combo-box%
  (class/cells vanilla-set-selector% () 
    (inherit get-editor get-value set-value!)
    
    ; Fields -------------------------------------    
    
    ; (alistof (U boolean integer symbol) . string)
    (init-cell available-items null #:override-accessor #:mutator)
    
    ; combo-box%
    (super-new [editor (new combo-box% [classes (list "editor")])])
    
    (refresh-selectable-items)
    
    ; Methods ------------------------------------
    
    ; -> (U number symbol)
    (define/override (get-editor-value) 
      (send (get-editor) get-value))
    
    ; -> void
    (define/override (reset-editor-value) 
      (void)) ; default for editor is fine
    
    ; any -> (U boolean integer symbol)
    (define/override (item->raw item)
      (car item))
    
    ; (U boolean integer symbol) -> any
    (define/override (raw->item raw)
      (assoc raw (get-available-items)))
    
    ; any -> string
    (define/override (item->string item)
      (cdr item))
    
    ; -> boolean
    (define/override (items-available?)
      (< (length (get-value)) (length (get-available-items))))
    
    ; -> void
    (define/override (refresh-selectable-items)
      (let ([selected-items (get-value)])
        (send (get-editor) set-options! 
              (filter (lambda (item) (not (member item selected-items)))
                      (get-available-items)))))))


; Provides ---------------------------------------

(provide set-selector-combo-box%)
