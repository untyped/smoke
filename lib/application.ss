#lang scheme

(require file/md5
         (only-in web-server/dispatchers/dispatch next-dispatcher)
         "../lib-base.ss"
         "component.ss")

(define-class application% component% (application<%>)
  
  ; Fields -------------------------------------
  
  (init-field page)
  
  ; (cell (U string #f))
  (init-cell web-frame-serial #f
    #:accessor #:mutator)
  
  ; Dispatching requests -----------------------
  
  ; -> response
  (define/public (dispatch)
    (let* ([request (current-request)]
           [url     (request-uri request)])
      ; Create or restore web frame:
      (if (callback-url? url)
          (begin (debug "request-serial" (request-serial))
                 (update-web-frame! (load-web-frame (request-serial)))
                 (unless (ajax-request? (current-request))
                   (set-web-frame-serial! (generate-serial))
                   (debug "reset-serial" (get-web-frame-serial))))
          (begin (clear-web-frame!)
                 (set-web-frame-serial! (generate-serial))
                 (debug "init-serial" (get-web-frame-serial))))
      (begin0
        ; Dispatch the request and retrieve the response:
        (if (callback-url? url)
            (dispatch-callback url)
            (dispatch-initial  url))
        ; Save the web frame:
        (debug "save-serial" (get-web-frame-serial))
        (save-web-frame!
         (get-web-frame-serial)
         (capture-web-frame)))))
  
  ; url -> response
  (define/public (dispatch-initial url)
    (match (url-path-base url)
      [(list) (current-page-set! page)
              (send page respond)]
      [_      (next-dispatcher)]))
  
  ; callback -> response
  (define/public (dispatch-callback url)
    (let*-values ([(callback)  (url->callback url this)]
                  [(page comp) (find-page+component (callback-component-id callback))])
      (current-page-set! page)
      (send-callback comp (callback-method-id callback) (callback-args callback))))
  
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

; Web frame serial codes -------------------------

; natural
(define serial-seed1
  (current-inexact-milliseconds))

; -> string
(define (generate-serial)
  (let ([serial-seed2 (current-inexact-milliseconds)])
    (bytes->string/utf-8
     (md5 (string->bytes/utf-8
           (string-append (number->string serial-seed1)
                          (number->string serial-seed2)))))))

; -> string
(define (request-serial)
  (request-binding-ref (current-request) '__k))

; string web-frame -> void
(define (save-web-frame! serial frame)
  (with-output-to-file (web-frame-path serial)
    (lambda ()
      (write (serialize frame)))))

; string -> web-frame
(define (load-web-frame serial)
  (with-input-from-file (web-frame-path serial)
    (lambda ()
      (deserialize (read)))))

; string -> path
(define (web-frame-path serial)
  (build-path (current-directory)
              (format "~a.webframe" serial)))

; Provides ---------------------------------------

(provide application%)