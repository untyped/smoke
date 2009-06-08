#lang scheme/base

(require web-server/dispatchers/dispatch
         (planet untyped/dispatch:3)
         "../web-server/dispatch3-wrapper.ss"
         (except-in "base.ss"
                    string-arg
                    symbol-arg
                    integer-arg
                    real-arg))

; Site -------------------------------------------

(define-site testapp
  [("")                                           home]
  [("autocomplete")                               autocomplete]
  [("counter")                                    counter]
  [("refresh-counter")                            refresh-counter]
  [("current-request")                            test-current-request]
  [("dialog")                                     dialog]
  [("editor")                                     editor]
  [("form")                                       form]
  [("form" "checked")                             checked-form]
  [("form" "hidden")                              form/hidden]
  [("notification1")                              notification1]
  [("notification2")                              notification2]
  [("redirect")                                   redirect]
  [("requirements")                               requirements]
  [("scroll")                                     scroll]
  [("segfault")                                   segfault]
  [("session")                                    test-session-show]
  [("session" "set"    (symbol-arg) (string-arg)) test-session-set]
  [("session" "remove" (symbol-arg))              test-session-remove]
  [("session" "start"  (boolean-arg))             test-session-start]
  [("session" "end"    (boolean-arg))             test-session-end]
  [("tab")                                        tab]
  [("tooltip")                                    tooltip])

; Controllers ------------------------------------

; request -> response
(define-controller (home request)
  (make-html-response
   (xml (html (head (title "Smoke test application"))
              (body (h1 "Smoke test application")
                    (ul ,@(for/list ([controller (in-list (reverse (site-controllers testapp)))])
                            (with-handlers ([exn? (lambda _ (xml))])
                              (opt-xml (not (eq? controller home))
                                (li (a (@ [href ,(testapp-url controller)])
                                       ,(controller-id controller))))))))))))

; Provide statements -----------------------------

(provide (site-out testapp))
