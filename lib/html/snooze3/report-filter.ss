#lang scheme/base

(require scheme/contract
         (planet untyped/unlib:3/symbol)
         "../../../lib-base.ss"
         "../combo-box.ss")

; Structure types --------------------------------

; (struct symbol string)
(define-struct filter (id name))

; Components -------------------------------------

(define filter-combo-box%
  (class/cells vanilla-combo-box% ()
    
    ; Fields -------------------------------------
    
    (init-field report #f)
    
    ; Methods ------------------------------------
    
    ; -> (listof filter)
    (define/override (get-options)
      (send report get-filters))
    
    ; (U filter #f) -> (U string #f)
    (define/override (option->raw filter)
      (symbol->string (filter-id filter)))
    
    ; (U string #f) -> (U filter #f)
    (define/override (raw->option raw-string)
      (and raw-string
           (let ([raw-symbol (string->symbol raw-string)])
             (ormap (lambda (filter)
                      (and (eq? raw-symbol (filter-id filter)) filter))
                    (get-options)))))
    
    ; filter -> string
    (define/override (option->string filter)
      (format "Search ~a" (filter-name filter)))))

; Provide statements -----------------------------

(provide filter-combo-box%)

(provide/contract
 [struct filter ([id symbol?] [name string?])])
