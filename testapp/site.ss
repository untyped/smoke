#lang scheme/base

(require "base.ss")

; Site -------------------------------------------

(define-site test-site site%
  ([()                                home-page]
   [("/autocomplete")                 autocomplete-page]
   [("/counter")                      counter-page]
   [("/refresh-counter")              refresh-counter-page]
   [("/current-request")              current-request-page]
   [("/dialog")                       dialog-page]
   [("/focus")                        focus-page]
   [("/form")                         form-page]
   [("/form/" (boolean-arg))          form-page]
   [("/form/checked")                 checked-form-page]
   [("/notification1")                notification-page1]
   [("/notification2")                notification-page2]
   [("/redirect")                     redirect-page]
   [("/requirements")                 requirements-page]
   [("/scroll")                       scroll-page]
   ;[("/session")                      session-page]
   ;[("/session/set/" (symbol-arg) "/" (string-arg)) session-set-page]
   ;[("/session/remove/" (symbol-arg)) session-remove-page]
   ;[("/session/start/" (boolean-arg)) session-start-page]
   ;[("/session/end")                  session-end-page]
   [("/tab")                          tab-page]
   [("/tooltip")                      tooltip-page]))

; Controllers ------------------------------------

; request -> response
(define-page home-page html-page% ()
  
  ; seed -> xml
  (define/augment (render seed)
    (xml (h1 "Smoke test site")
         (ul ,@(for/list ([page (in-list (reverse (send test-site get-pages)))])
                 (with-handlers ([exn? (lambda _ (xml))])
                   (opt-xml (not (eq? page home-page))
                     (li (a (@ [href ,(controller-url page)])
                            ,(send page get-component-id))))))))))

; Provide statements -----------------------------

(provide (site-out test-site))
