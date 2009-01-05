#lang scheme/base

(require net/url
         scheme/contract
         "web-server/continuation-url.ss")

; Procedures -------------------------------------

; url -> url
(define (smoke-url-cleaner url)
  (make-url #f #f #f #f #t 
            (map (lambda (path/param)
                   (make-path/param (path/param-path path/param) null))
                 (url-path-base (url-path url)))
            (url-query url)
            (url-fragment url)))

; Provide statements -----------------------------

(provide/contract
 [smoke-url-cleaner (-> url? url?)])
