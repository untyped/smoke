#lang scheme/base

(require "../../lib-base.ss")

(require "autocomplete-field.ss"
         "set-selector.ss")


(define set-selector-autocomplete%
  (class/cells vanilla-set-selector% ()    
    
    (inherit get-editor set-editor! get-value set-value! get-id)
    
    ; Fields -------------------------------------
    
    ; (alistof (U boolean integer symbol) . string)
    (init-cell available-items null #:override-accessor)
    
    ; Constructor --------------------------------
    
    ; autocomplete-field% 
    (init [autocomplete (new autocomplete-field% [classes '(editor)])]) 
    
    (super-new [editor autocomplete])
    
    ; Methods ------------------------------------ 
    
    ; -> (U number symbol)
    (define/override (get-editor-value) 
      (let ([string-value (send (get-editor) get-value)])
        (for/or ([item (in-list (get-available-items))])
          (and (equal? string-value (item->string item)) (item->raw item)))))
    
    ; -> void
    (define/override (reset-editor-value) 
      (send (get-editor) set-value! #f))
    
    ; -> (listof (alistof (U boolean symbol number) string))
    (define/public (set-available-items! items)
      (web-cell-set! available-items-cell items)
      (refresh-selectable-items))
    
    ; autocomplete-field% -> void
    (define/public (set-autocomplete-field! autocomplete)
      (set-editor! autocomplete))
    
    ; (cons (U boolean integer symbol) . string) -> (U boolean integer symbol)
    (define/override (item->raw item)
      (car item))
    
    ; (U boolean integer symbol) -> (cons (U boolean integer symbol) . string)
    (define/override (raw->item raw)
      (assoc raw (get-available-items)))
    
    ; (cons (U boolean integer symbol) . string) -> string
    (define/override (item->string item)
      (cdr item))
    
    ; -> boolean
    (define/override (items-available?)
      (< (length (get-value)) (length (get-available-items))))
    
    ; -> void
    (define/override (refresh-selectable-items)
      (let ([selected-items (get-value)])
        (send (get-editor) set-options! 
              (map (cut item->string <>)
                   (filter (lambda (item) (not (member item selected-items))) (get-available-items))))))))

; Provides ---------------------------------------

(provide set-selector-autocomplete%)