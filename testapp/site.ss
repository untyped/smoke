#lang scheme/base

(require "base.ss")

; Site -------------------------------------------

(define-site test-site
  ([("")                              home]
   [("/autocomplete")                 autocomplete]
   [("/counter")                      counter]
   [("/refresh-counter")              refresh-counter]
   [("/current-request")              test-current-request]
   [("/dialog")                       dialog]
   [("/focus")                        focus]
   [("/form")                         form]
   [("/form/checked")                 checked-form]
   [("/form/hidden")                  form/hidden]
   [("/notification1")                notification1]
   [("/notification2")                notification2]
   [("/redirect")                     redirect]
   [("/requirements")                 requirements]
   [("/scroll")                       scroll]
   [("/segfault")                     segfault]
   [("/session")                      test-session-show]
   [("/session/set/" (symbol-arg) "/" (string-arg)) test-session-set]
   [("/session/remove/" (symbol-arg)) test-session-remove]
   [("/session/start/" (boolean-arg)) test-session-start]
   [("/session/end")                  test-session-end]
   [("/tab")                          tab]
   [("/tooltip")                      tooltip])
  #:requestless? #t)

(default-controller-wrapper
  (lambda (controller . args)
    (init-smoke
     (lambda ()
       (apply plain-controller-wrapper controller args)))))

; Controllers ------------------------------------

; request -> response
(define-controller (home)
  (make-html-response
   (xml (html (head (title "Smoke test application"))
              (body (h1 "Smoke test application")
                    (ul ,@(reverse (for/list ([controller (site-controllers test-site)])
                                     (with-handlers ([exn? (lambda _ (xml))])
                                       (if (eq? controller home)
                                           (xml)
                                           (xml (li (a (@ [href ,(controller-url controller)])
                                                       ,(controller-id controller))))))))))))))

; Provide statements -----------------------------

(provide (site-out test-site))
