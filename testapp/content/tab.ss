#lang scheme/base

(require (planet untyped/unlib:3/string)
         "../content-base.ss")

; Controllers ------------------------------------

; request -> response
(define-controller tab
  init-smoke-pipeline
  (lambda (request)
    (send tab-page respond)))

; Components -------------------------------------

(define tab-page
  (singleton/cells html-page% ()
    
    ; Fields -------------------------------------
    
    ; text-area%
    (field [editor (new text-area%
                        [id        'editor]
                        [value     "Content"]
                        [on-change (callback on-editor-change)])] 
      #:accessor #:mutator)
    
    ; html-range%
    (field [preview (static-range (xml ,(send editor get-value)))]
      #:accessor #:mutator)
    
    ; tab-pane%
    (field [tab-pane 
            (new tab-pane%
                 [tabs (list (new tab%
                                  [label   (xml "Editor")]
                                  [content editor])
                             (new tab%
                                  [label   (xml "Preview")]
                                  [content preview]))])]
      #:child #:accessor #:mutator)
    
    ; Constructor --------------------------------
    
    (super-new [title "Tab pane"])
    
    ; Methods ------------------------------------
    
    ; -> void
    (define/public #:callback (on-editor-change)
      (send preview set-content! (xml ,(send editor get-value))))
    
    ; seed -> xml
    (define/augment (render seed)
      (send tab-pane render seed))))
