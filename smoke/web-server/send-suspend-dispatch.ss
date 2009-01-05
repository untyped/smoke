#lang scheme/base

(require net/url
         scheme/contract
         scheme/pretty
         web-server/http/request-structs
         web-server/http/response-structs
         (planet untyped/unlib:3/debug)
         (planet untyped/unlib:3/log)
         "current-request.ss"
         "request-util.ss"
         "resume.ss"
         (prefix-in ws: "web.ss")
         "web-cell.ss")

; Embedding thunks as URLs -----------------------

; ((request -> response) -> string) boolean -> (thunk -> string)
;
; Creates a continuation URL that, when visited, runs thunk and returns void.
(define (make-derived-embed-url embed-url push?)
  (lambda (thunk)
    (define resume-k #f)

    (define frame (current-frame))
    
    (define url
      (embed-url
       (lambda (request)
         (current-request-set! request)
         
         (current-frame frame)
         
         (log-debug* "Continuation" (url->string (request-uri (current-request))))
         
         (cond [resume-k
                (current-frame frame)
                (resume-k)]
               [else (parameterize ([resume-parameter 
                                     (lambda (k)
                                       (set! resume-k k)
                                       (set! frame (current-frame)))])
                       (call-with-continuation-prompt
                        (lambda ()
                          (when push?
                            (current-frame (push-frame frame)))
                          (thunk))
                        resume-prompt))]))))
    
    url))

; ((thunk -> string) -> response) boolean -> ((thunk -> string) -> response)
(define (make-derived-response-generator response-generator push?)
  (if push?
      ; With conventional requests, we can leave the web frames as they are:
      response-generator
      ; With AJAX requests, we have to fiddle with the web frames
      ; once the response has been generated:
      (lambda (derived-embed-url)
        (begin0
          (response-generator derived-embed-url)
          (frame-squeeze! (current-frame))))))

; respond/dispatch -------------------------------

; (((thunk -> string) -> response) -> any)
(define (send/suspend/dispatch response-generator #:push-frame? [push? #t])
  
  ; ((thunk -> string) -> response)
  (define derived-response-generator
    (make-derived-response-generator response-generator push?))
  
  ; any
  (ws:send/suspend/dispatch
   (lambda (embed-url)
     
     ; thunk -> string
     (define derived-embed-url
       (make-derived-embed-url embed-url push?))
     
     (derived-response-generator derived-embed-url))))

; Provide statements -----------------------------

; Contract temporarily loosened to N arguments for debugging purposes:
(define smoke-thunk/c
  (-> any))

(provide/contract
 [send/suspend/dispatch (->* ((-> (-> any/c string?) response?)) (#:push-frame? boolean?) any)])
