#lang scheme/base

(require net/url
         srfi/13
         "../base.ss"
         "struct.ss")

; web-resource -> xml
(define (render-web-resource res)
  (xml ,@(for/fold ([accum null])
                   ([res (in-list (web-resource-plan res))])
                   (let ([block (render-single-web-resource res)])
                     (if block (cons block accum) accum)))))

; web-resource -> (U xml #f)
(define (render-single-web-resource res)
  (cond [(js-resource? res)
         (let ([src (url->string (js-resource-url res))])
           (xml (script (@ [type "text/javascript"]
                           [src ,src]))))]
        [(css-resource? res)
         (let* ([ie-selector (css-resource-ie-selector res)]
                [media-list  (css-resource-media res)]
                [media       (and (pair? media-list)
                                  (string-join media-list ","))]
                [href        (url->string (css-resource-url res))])
           (xml ,(opt-xml ie-selector
                   (!raw ,(format "\n<!--[if ~a]>\n" ie-selector)))
                (link (@ [rel "stylesheet"]
                         [type "text/css"]
                         ,(opt-xml-attr media)
                         [href ,href]))
                ,(opt-xml ie-selector
                   (!raw "\n<[endif]-->\n"))))]
        [else #f]))

; Helpers ----------------------------------------

; Provide statements -----------------------------

(provide/contract
 [render-web-resource (-> web-resource? xml?)])
