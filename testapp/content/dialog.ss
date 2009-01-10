#lang scheme/base

(require (planet untyped/unlib:3/string)
         "../content-base.ss")

; Controllers ------------------------------------

; request -> response
(define-controller dialog
  init-smoke-pipeline
  (lambda (request)
    (send main-page respond)))

; Components -------------------------------------

(define main-page
  (singleton/cells html-page% ()
    
    (inherit set-dialog!)
    
    ; Fields -------------------------------------
    
    ; text-area%
    (field [editor (new text-area%
                        [id 'editor]
                        [value "Content"])]
      #:child #:accessor #:mutator)
    
    ; Constructor --------------------------------
    
    (super-new [title "Dialog test"])
    
    ; Methods ------------------------------------
    
    ; seed -> xml
    (define/augment (render seed)
      (xml (div ,(send editor render seed))
           (div (a (@ [onclick ,(embed/ajax seed (callback on-show-dialog))]) "Preview"))))
    
    ; -> void
    (define/public #:callback (on-show-dialog)
      (send preview-dialog set-content! (send editor get-value))
      (set-dialog! preview-dialog))
    
    ; -> void
    (define/public #:callback (on-hide-dialog)
      (set-dialog! #f))))

(define preview-dialog
  (singleton/cells html-dialog% ()
    
    ; Fields -------------------------------------
    
    ; (cell (U string #f))
    (cell [content #f] #:accessor #:mutator)
    
    ; Constructor --------------------------------
    
    (super-new)
    
    ; Methods ------------------------------------
    
    ; seed -> xml
    (define/augment (render seed)
      (xml (div (@ [style "border: 1px solid #aaa; padding: 5px; margin: 5px"])
                (!raw ,(get-content)))
           (div (@ [style "margin: 5px"])
                (a (@ [onclick ,(embed/ajax seed (callback [main-page on-hide-dialog]))]) "Dismiss"))))))
