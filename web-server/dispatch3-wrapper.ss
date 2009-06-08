#lang scheme/base

(require "../base.ss")
         
(require (planet untyped/dispatch:3)
         "current-request.ss"
         "resume.ss"
         "send-suspend-dispatch.ss"
         "session.ss")

(current-controller-wrapper
 (lambda (continue controller request . args)
   (define (inner-continue)
     (apply continue controller (current-request) args))
   
   (define (outer-continue)
     (if (resume-available?)
         (begin
           (current-request-set! request)
           (inner-continue))
         (parameterize ([current-frame (push-frame)])
           (send/suspend/dispatch
            (lambda (embed-url)
              (make-redirect-response
               (embed-url inner-continue)))))))
   
   (if (request-session (current-request))
       (outer-continue)
       (start-session
        #f
        (lambda (session)
          (outer-continue))))))
