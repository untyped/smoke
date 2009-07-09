#lang scheme/base

(require (planet untyped/unlib:3/string)
         "../content-base.ss")

; Controllers ------------------------------------

; request -> response
(define-controller tab
  init-smoke-pipeline
  (lambda ()
    (send tab-page respond)))

; Components -------------------------------------

(define tab-page
  (singleton/cells html-page% ()
    
    ; Fields -------------------------------------
    
    ; text-area%
    (field editor
      (new text-area%
           [id        'editor]
           [value     "Content"]
           [on-change (callback on-editor-change)]))
    
    (field inline-preview        (new html-range% [id 'inline-preview]        [content (send editor get-value)]))
    (field demand-preview        (new html-range% [id 'demand-preview]        [content (send editor get-value)]))
    (field nested-inline-preview (new html-range% [id 'nested-inline-preview] [content (send editor get-value)]))
    (field nested-demand-preview (new html-range% [id 'nested-demand-preview] [content (send editor get-value)]))
    
    (field nested-tab-pane
      (new tab-pane%
           [id   'nested-tab-pane]
           [tabs (list (new tab%
                            [id      'tab41]
                            [label   "Nested inline preview"]
                            [content nested-inline-preview]
                            [inline? #t])
                       (new tab%
                            [id      'tab42]
                            [label   "Nested on-demand preview"]
                            [content nested-demand-preview]
                            [inline? #f]))]))
    
    ; tab-pane%
    (field tab-pane
      (new tab-pane%
           [id   'tab-pane]
           [tabs (list (new tab%
                            [id      'tab1]
                            [label   "Editor"]
                            [content editor]
                            [inline? #t])
                       (new tab%
                            [id      'tab2]
                            [label   "Inline preview"]
                            [content inline-preview]
                            [inline? #t])
                       (new tab%
                            [id      'tab3]
                            [label   "On-demand preview"]
                            [content demand-preview]
                            [inline? #f])
                       (new tab%
                            [id      'tab4]
                            [label   "Nested previews"]
                            [content nested-tab-pane]
                            [inline? #t]))])
      #:child #:accessor)
    
    ; Constructor --------------------------------
    
    (super-new [title "Tab pane"])
    
    ; Methods ------------------------------------
    
    ; -> void
    (define/public #:callback (on-editor-change)
      (send inline-preview set-content! (xml ,(send editor get-value)))
      (send demand-preview set-content! (xml ,(send editor get-value)))
      (send nested-inline-preview set-content! (xml ,(send editor get-value)))
      (send nested-demand-preview set-content! (xml ,(send editor get-value))))
    
    ; seed -> xml
    (define/augment (render seed)
      (send tab-pane render seed))))
