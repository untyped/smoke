#lang scheme/base

(require srfi/26
         "../test-base.ss"
         "render.ss"
         "struct.ss"
         (prefix-in internal: "struct-internal.ss"))

; Test data --------------------------------------

(define-js-resource jquery-script
  "/scripts/jquery/jquery.js")

(define-js-resource mysite-script
  "/scripts/mysite/mysite.js"
  (jquery-script))

(define-css-resource common-styles
  "/styles/mysite/common.css")

(define-css-resource screen-styles
  "/styles/mysite/screen.css"
  (common-styles)
  #:media (list "screen" "projection"))

(define-css-resource print-styles
  "/styles/mysite/print.css"
  #:media (list "print"))

(define-css-resource ie-styles
  "/styles/mysite/ie.css"
  (screen-styles common-styles)
  #:media (list "screen")
  #:ie-version "IE")

(define-compound-resource mysite-styles
  (common-styles screen-styles print-styles ie-styles))

; Tests ------------------------------------------

(define render-tests
  (test-suite "render.ss"
    
    (test-equal? "render js resource"
      (xml->string (render-web-resource mysite-script))
      (xml->string (xml (script (@ [type "text/javascript"]
                                   [src  "/scripts/jquery/jquery.js"]))
                        (script (@ [type "text/javascript"]
                                   [src  "/scripts/mysite/mysite.js"])))))
    
    (test-equal? "render css resource"
      (xml->string (render-web-resource mysite-styles))
      (xml->string (xml (link (@ [rel   "stylesheet"]
                                 [type  "text/css"]
                                 [href  "/styles/mysite/common.css"]))
                        (link (@ [rel   "stylesheet"]
                                 [type  "text/css"]
                                 [media "screen,projection"]
                                 [href  "/styles/mysite/screen.css"]))
                        (link (@ [rel   "stylesheet"]
                                 [type  "text/css"]
                                 [media "print"]
                                 [href  "/styles/mysite/print.css"]))
                        (!raw "\n<!--[if IE]>\n")
                        (link (@ [rel   "stylesheet"]
                                 [type  "text/css"]
                                 [media "screen"]
                                 [href  "/styles/mysite/ie.css"]))
                        (!raw "\n<[endif]-->\n"))))))

; Provide statements -----------------------------

(provide render-tests)
