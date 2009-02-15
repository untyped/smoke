#lang scheme/base

(require web-server/dispatchers/dispatch
         (planet untyped/dispatch:1)
         "base.ss")

; Site -------------------------------------------

(define-site test-site
  ([(url "")                              home]
   [(url "/autocomplete")                 autocomplete]
   [(url "/counter")                      counter]
   [(url "/current-request")              test-current-request]
   [(url "/dialog")                       dialog]
   [(url "/editor")                       editor]
   [(url "/form")                         form]
   [(url "/form/checked")                 checked-form]
   [(url "/form/hidden")                  form/hidden]
   [(url "/notification1")                notification1]
   [(url "/notification2")                notification2]
   [(url "/redirect")                     redirect]
   [(url "/requirements")                 requirements]
   [(url "/scroll")                       scroll]
   [(url "/segfault")                     segfault]
   [(url "/session")                      test-session-show]
   [(url "/session/set/"
         (symbol-arg) "/" (string-arg))   test-session-set]
   [(url "/session/remove/" (symbol-arg)) test-session-remove]
   [(url "/session/start/" (boolean-arg)) test-session-start]
   [(url "/session/end/" (boolean-arg))   test-session-end]
   [(url "/tab")                          tab]
   [(url "/tooltip")                      tooltip])
  #:rule-not-found
  (lambda (request)
    (next-dispatcher)))

; Controllers ------------------------------------

; request -> response
(define-controller home
  null
  (lambda (request)
    (make-html-response
     (xml (html (head (title "Smoke test application"))
                (body (h1 "Smoke test application")
                      (ul ,@(reverse (for/list ([controller (site-controllers test-site)])
                                       (with-handlers ([exn? (lambda _ (xml))])
                                         (if (eq? controller home)
                                             (xml)
                                             (xml (li (a (@ [href ,(controller-url controller)])
                                                         ,(controller-id controller)))))))))))))))
  
; Provide statements -----------------------------

(provide (site-out test-site))
