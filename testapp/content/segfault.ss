#lang scheme/base

(require (for-syntax scheme/base)
         scheme/runtime-path
         (planet untyped/unlib:3/yield)
         "../content-base.ss")

; NOTES SPECIFIC TO THIS NEW PROBLEM:
;
; This new problem is simpler than the last. There's no database connection this time:
; just use of a simple library function in unlib.plt/yield.ss, that implements the "yield"
; operator from Ruby and Python.
; 
; The seg fault only occurs when using my original implementation of yield, which is written
; with full continuation jumps. I've since rewritten it to use composable continuations and
; its own continuation prompt. This fixes the problem.
; 
; The two versions of yield are available as:
;
;     make-yieldable/full-continuations
;     make-yieldable/composable-continuations
;
; The documentation for yield is in plt-help.

; NOTES FROM THE ORIGINAL DATABASE-RELATED PROBLEM THAT ARE STILL RELEVANT:
; 
; This test scenario consists of:
;
; - A "controller" procedure called "segfault". Note that the define-controller form does
;   not actually expand into a define form... "segfault" is defined in "../site.ss" and 
;   define-controller merely mutates the definition.
;
; - A singleton page object that creates an HTML page with a form on it. When someone
;   clicks the submit button, the browser POSTs the (empty) form data to the web server
;   and the page object immediately triggers a redirect to a continuation. This prevents
;   those irritating "would you like to submit your form contents again" problems when
;   using the back button.
;
; The code here all relies on an implementation of send/suspend/dispatch written in
; "smoke/web-server/send-suspend-dispatch.ss". The definition uses a delimited continuation ("resume-k")
; so that hitting "reload" on a page merely redisplays the same data (rather than 
; re-performing whatever update operation was originally attached to the URL they are 
; viewing).

; Controllers ------------------------------------

; request -> response
(define-controller (segfault request)
  (send test-page respond))

; Components -------------------------------------

(define test-page
  (singleton/cells html-page% ()
    
    ; Fields -------------------------------------
    
    ; submit-button%
    (field [submit (new submit-button%
                        [id     'submit-button]
                        [label  "Submit"]
                        [action (callback on-submit)])]
      #:child #:accessor #:mutator)
    
    ; Constructor --------------------------------
    
    (super-new [title "Segfault test"])
    
    ; Methods ------------------------------------
    
    ; seed -> xml
    (define/augment (render seed)
      (xml (p "There should be a list 1,2,3,4,5,1,2,3,4,5 below:")
           (ul (li ,(format "~a" (generate-data)))
               (li ,(format "~a" (generate-data)))
               (li ,(format "~a" (generate-data)))
               (li ,(format "~a" (generate-data)))
               (li ,(format "~a" (generate-data)))
               (li ,(format "~a" (generate-data)))
               (li ,(format "~a" (generate-data)))
               (li ,(format "~a" (generate-data)))
               (li ,(format "~a" (generate-data)))
               (li ,(format "~a" (generate-data))))
           ,(send submit render seed)))
    
    ; -> void
    (define/public #:callback (on-submit)
      (void))))

; Helpers ----------------------------------------

; -> xml
#;(define generate-data
  (; make-yieldable/full-continuations
   ; Swapping the procedure above for the one below prevents the segfault:
   make-yieldable/composable-continuations
   (lambda (yield)
     (lambda ()
       (yield 1)
       (yield 2)
       (yield 3)
       (yield 4)
       5))))

; -> xml
(define generate-data
    (; make-yieldable/full-continuations
     ; Swapping the procedure above for the one below prevents the segfault:
     make-yieldable/composable-continuations
     (lambda (yield)
       (lambda ()
         (yield (list 1 2))
         (yield (list 2 3))
         (yield (list 3 4))
         (yield (list 4 5))
         (list 5 6)))))

; Dummy database connection ----------------------

; path
(define-runtime-path database-path "../../dummy-test-database.txt")

; (thread-cell (U output-port #f))
(define connection-cell (make-thread-cell #f))

; thunk -> any
(define (call-with-database-connection thunk)
  (dynamic-wind
   (lambda ()
     (unless (thread-cell-ref connection-cell)
       (thread-cell-set! 
        connection-cell 
        (open-output-file database-path #:exists 'replace))))
   thunk
   (lambda ()
     (when (thread-cell-ref connection-cell)
       (close-output-port (thread-cell-ref connection-cell))
       (thread-cell-set! connection-cell #f)))))
