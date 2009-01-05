#lang scheme/base

(require scheme/contract
         (planet untyped/unlib:3/symbol)
         "../../../lib-base.ss"
         "../combo-box.ss"
         "report-column.ss")

; Structure types --------------------------------

; (struct symbol string (listof column))
(define-struct view (id name columns) #:transparent)

; Components -------------------------------------

(define view-combo-box%
  (class/cells vanilla-combo-box% ()
    
    ; Fields -------------------------------------
    
    ; snooze-report%
    (init-field [report #f] #:accessor #:mutator)
    
    ; Methods ------------------------------------
    
    ; -> (listof view)
    (define/override (get-options)
      (send report get-views))
    
    ; view -> string
    (define/override (option->raw view)
      (symbol->string (view-id view)))
    
    ; (U string #f) -> (U view #f)
    (define/override (raw->option raw-string)
      (and raw-string
           (let ([raw-symbol (string->symbol raw-string)])
             (ormap (lambda (view)
                      (and (eq? raw-symbol (view-id view)) view))
                    (get-options)))))
    
    ; view -> string
    (define/override (option->string view)
      (view-name view))))

; Provide statements -----------------------------

(provide view-combo-box%)

(provide/contract
 [struct view ([id symbol?] [name string?] [columns (listof (is-a?/c snooze-report-column%))])])
