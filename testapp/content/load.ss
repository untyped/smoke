#lang scheme

(require "../content-base.ss")

(define-page load-page html-page% ()
  
  (inherit get-child-components)
  
  (super-new [title "Load"])
  
  (field fields 
    (for/list ([i (in-range 100)])
      (new text-field%
           [id (string->symbol (format "field~a" i))]
           [on-change (callback on-change)]))
    #:children)
    
  (define/augment (render seed)
    (xml (div "Combined entries from all fields:")
         (div ,@(for/list ([field (in-list fields)])
                  (xml ,(send field get-value) " ")))
         (div ,@(for/list ([field (in-list fields)])
                  (send field render seed)))))
  
  (define/override (dirty?)
    (ormap (cut send <> dirty?)
           (get-child-components)))
  
  (define/public #:callback (on-change)
    (void)))

         
    