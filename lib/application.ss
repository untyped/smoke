#lang scheme

(require "../lib-base.ss"
         "component.ss")

(define-class application% component% (application<%>)
  
  (init-field page)
  
  ; Dispatching requests -----------------------
  
  ; -> response
  (define/public (dispatch)
    (let* ([request (current-request)]
           [url     (request-uri request)])
      (if (callback-url? url)
          (dispatch-callback (url->callback url this))
          (dispatch-initial  url))))
  
  ; callback -> response
  (define/public (dispatch-callback callback)
    (debug "callback" callback)
    (let-values ([(page comp) (find-page+component (callback-component-id callback))])
      (current-page-set! page)
      (send comp call-callback
            (callback-method-id callback)
            (callback-args callback))))
  
  ; url -> response
  (define/public (dispatch-initial url)
    (debug "initial" (url->string url))
    (current-page-set! page)
    (send page respond))
  
  ; symbol -> (U page<%> #f) (U component<%> #f)
  (define/public (find-page+component id)
    (let loop ([children (list page)])
      (match children
        [(list) (values #f #f)]
        [(list-rest page rest)
         (let ([comp (send page find-component id)])
           (if comp
               (values page comp)
               (loop rest)))]))))

; Provides ---------------------------------------

(provide application%)