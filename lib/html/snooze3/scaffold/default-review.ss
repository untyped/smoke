#lang scheme/base

(require "../../../../lib-base.ss"
         "../../html-element.ss"
         "default-abstract.ss"
         "interfaces.ss")

; Mixins -----------------------------------------

; Defaults for review elements
(define (default-review-mixin)
  (mixin/cells (crudl-review+delete<%>) 
    (crudl-review+delete<%>)
    
    (inherit get-value render-struct)
    
    ; seed -> xml
    (define/augment (render seed)
      (render-struct seed (get-value)))))

; DJG : There's also an is-a?/c

(define review-mixin/c
  (-> (implementation?/c crudl-review+delete<%>)
      (implementation?/c crudl-review+delete<%>)))

; Provides ---------------------------------------
(provide/contract
 [default-review-mixin (-> review-mixin/c)])