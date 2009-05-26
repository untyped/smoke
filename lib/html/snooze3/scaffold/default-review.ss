#lang scheme/base

(require "../../../../lib-base.ss"
         "../../html-element.ss"
         "default-abstract.ss"
         "scaffold-internal.ss")

; Mixins -----------------------------------------

; Defaults for review elements
(define (default-review-mixin)
  (mixin/cells (html-element<%> crud-element<%> crudl-element<%> crudl-review+delete+list<%>) 
    (crudl-review+delete<%>)
    
    (inherit get-struct render-struct)
    
    ; seed -> xml
    (define/augment (render seed)
      (render-struct seed (get-struct)))))

; Provides ---------------------------------------
(provide (all-defined-out))