#lang scheme/base

(require (only-in web-server/managers/manager manager?)
         web-server/managers/lru
         (planet untyped/mirrors:1/mirrors)
         "all-smoke-tests.ss"
         "test-base.ss")

; Servlet stuff --------------------------------

(define interface-version 'v2)

; request -> response
; The library tests itself entirely by continuation:
; this servlet serves no real purpose.
(define (start initial-request)
  (make-html-response
   (xml (html (head (title "Tests not running"))
              (body (p "For some reason the Smoke test suite did not start."))))))

; request -> response
(define (instance-expiration-handler response)
  (error "Expired instance!"))

; manager
(define manager
  (let ([memory-threshold (* 32 1024 1024)])
    (create-LRU-manager
     instance-expiration-handler                    ; request -> response
     5                                              ; check condition X every 5 seconds
     (* 30 60)                                      ; deduct one life point every 30 minutes
     (cut >= (current-memory-use) memory-threshold) ; condition X
     #:initial-count 12                             ; 12 life points to start with
     #:inform-p void)))                             ; called when instances are collected

; Provide statements ---------------------------

(provide/contract
 [interface-version           symbol?]
 [start                       (-> request? response?)]
 [instance-expiration-handler (-> request? response?)]
 [manager                     manager?])
