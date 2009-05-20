#lang scheme/base

(require net/url
         scheme/contract
         srfi/26
         (except-in web-server/http
                    redirect-to)
         (except-in web-server/servlet
                    clear-continuation-table!
                    ; Web cell stuff:
                    web-cell?
                    web-cell-set?
                    make-web-cell
                    web-cell-ref
                    web-cell-shadow
                    ; Send procedures:
                    send/forward
                    send/suspend
                    send/suspend/dispatch
                    redirect-to
                    ; Request bindings:
                    exists-binding?
                    extract-binding/single
                    extract-bindings
                    ; web-server/dispatch:
                    symbol-arg
                    string-arg
                    integer-arg
                    real-arg)
         (planet untyped/mirrors:1)
         "current-request.ss"
         "request.ss"
         "request-util.ss"
         (only-in "resume.ss" resume-from-here)
         "send-suspend-dispatch.ss"
         (only-in "web.ss" clear-continuation-table!)
         "web-cell.ss")

; (U string url) -> void
(define (redirect-to url)
  (if (ajax-request? (current-request))
      (send/back (make-js-response (js (= (!dot window location) ,url))))
      (send/back (make-redirect-response url))))

; Provide statements -----------------------------

(provide (all-from-out web-server/http
                       web-server/servlet
                       "request.ss"
                       "request-util.ss"
                       "resume.ss"
                       "send-suspend-dispatch.ss"
                       "web-cell.ss"
                       "web.ss")
         ; From current-request.ss:
         current-request)

(provide/contract
 [redirect-to (-> (or/c url? string?) any)])
