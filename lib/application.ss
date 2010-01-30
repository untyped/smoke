#lang scheme

(require file/md5
         (only-in web-server/dispatchers/dispatch next-dispatcher)
         "../lib-base.ss"
         "component.ss")

(define-class application% component% (application<%>)
  
  ; Fields -------------------------------------
  
  (init-field page)
    
  ; Dispatching requests -----------------------
  
  ; -> response
  (define/public (dispatch)
    (let* ([request (current-request)]
           [url     (request-uri request)])
      (if (callback-url? url)
          (dispatch-callback url)
          (dispatch-initial  url))))
  
  ; url -> response
  (define/public (dispatch-initial url)
    (match (url-path-base url)
      [(list) (current-page-set! page)
              (with-saved-web-frame (send page respond))]
      [_      (next-dispatcher)]))
  
  ; callback -> response
  (define/public (dispatch-callback url)
    (let*-values ([(callback)  (url->callback url this)]
                  [(page comp) (find-page+component (callback-component-id callback))])
      (current-page-set! page)
      (with-saved-web-frame (send-callback comp (callback-method-id callback) (callback-args callback)))))
  
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