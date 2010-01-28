#lang scheme/base

(require (only-in srfi/13/string string-trim-both)
         "../../lib-base.ss"
         "../../test-base.ss"
         "html-component.ss")

; Tests ------------------------------------------

(define html-component-tests
  (test-suite "html-component.ss"
    
    '#:before
    (lambda ()
      ; Make sure the detach script has a frame to check itself in:
      (current-frame (push-frame)))
    
    (test-equal? "render : root frame"
      (xml->string (send n1 render #f)) 
      #<<ENDXML
<node id="n1"><node id="n2"><node id="n3"></node><node id="n4"></node></node><node id="n5"><node id="n6"></node><node id="n7"></node></node></node>
ENDXML
      )
    
    (test-case "render : nove moved"
      (call-with-frame (push-frame)
        (lambda ()
          (send n2 set-children! (list n3))
          (send n7 set-children! (list n4))
          (check-equal? (xml->string (send n1 render #f))
                        #<<ENDXML
<node id="n1"><node id="n2"><node id="n3"></node></node><node id="n5"><node id="n6"></node><node id="n7"><node id="n4"></node></node></node></node>
ENDXML
                        ))))
    
    (test-case "get-on-refresh : nothing changed"
      (call-with-frame (push-frame)
        (lambda ()
          (check-equal? (regexp-split #rx"[ \t\n\r]+" (string-trim-both (javascript->string (send n1 get-on-refresh #f))))
                        (list "detach(\"n1\");"
                              "detach(\"n2\");"
                              "detach(\"n3\");"
                              "detach(\"n4\");"
                              "detach(\"n5\");"
                              "detach(\"n6\");"
                              "detach(\"n7\");"
                              "render(\"n1\");"
                              "attach(\"n3\");"
                              "attach(\"n4\");"
                              "attach(\"n2\");"
                              "attach(\"n6\");"
                              "attach(\"n7\");"
                              "attach(\"n5\");"
                              "attach(\"n1\");")))))
    
    (test-case "get-on-refresh : nodes deleted"
      (call-with-frame (push-frame)
        (lambda ()
          (send n1 set-children! null)
          (check-equal? (regexp-split #rx"[ \t\n\r]+" (string-trim-both (javascript->string (send n1 get-on-refresh #f))))
                        (list "detach(\"n1\");"
                              "detach(\"n2\");"
                              "detach(\"n3\");"
                              "detach(\"n4\");"
                              "detach(\"n5\");"
                              "detach(\"n6\");"
                              "detach(\"n7\");"
                              "render(\"n1\");"
                              "attach(\"n1\");")))))
    
    (test-case "get-on-refresh : nodes added"
      (call-with-frame (push-frame)
        (lambda ()
          (send n1 set-children! null)
          (call-with-frame (push-frame)
            (lambda ()
              (send n1 set-children! (list n2 n5))
              (check-equal? (regexp-split #rx"[ \t\n\r]+" (string-trim-both (javascript->string (send n1 get-on-refresh #f))))
                            (list "detach(\"n1\");"
                                  "detach(\"n2\");"
                                  "detach(\"n3\");"
                                  "detach(\"n4\");"
                                  "detach(\"n5\");"
                                  "detach(\"n6\");"
                                  "detach(\"n7\");"
                                  "render(\"n1\");"
                                  "attach(\"n3\");"
                                  "attach(\"n4\");"
                                  "attach(\"n2\");"
                                  "attach(\"n6\");"
                                  "attach(\"n7\");"
                                  "attach(\"n5\");"
                                  "attach(\"n1\");")))))))
    
    (test-case "get-on-refresh : node moved"
      (call-with-frame (push-frame)
        (lambda ()
          (send n2 set-children! (list n3))
          (send n7 set-children! (list n4))
          (check-equal? (regexp-split #rx"[ \t\n\r]+" (string-trim-both (javascript->string (send n1 get-on-refresh #f))))
                        (list "detach(\"n1\");"
                              "detach(\"n2\");"
                              "detach(\"n3\");"
                              "detach(\"n4\");"
                              "detach(\"n5\");"
                              "detach(\"n6\");"
                              "detach(\"n7\");"
                              "render(\"n1\");"
                              "attach(\"n3\");"
                              "attach(\"n2\");"
                              "attach(\"n6\");"
                              "attach(\"n4\");"
                              "attach(\"n7\");"
                              "attach(\"n5\");"
                              "attach(\"n1\");")))))
    
    (test-case "verify-callback-id"
      (check-exn exn:fail? (cut send n1 verify-callback-id 'does-not-exist))
      (check-pred symbol? (send n1 verify-callback-id 'on-noop/0))
      (check-pred symbol? (send n1 verify-callback-id 'on-noop/1))
      (check-pred symbol? (send n1 verify-callback-id 'on-noop/1-2))
      (check-pred symbol? (send n1 verify-callback-id 'on-noop/n)))
    
    (test-case "call-callback : correct arity"
      (check-equal? (send n1 call-callback 'on-noop/0 null) null)
      (check-equal? (send n1 call-callback 'on-noop/1 '(1)) (list 1))
      (check-equal? (send n1 call-callback 'on-noop/1-2 '(1)) (list 1 #f))
      (check-equal? (send n1 call-callback 'on-noop/1-2 '(1 2)) (list 1 2))
      (check-equal? (send n1 call-callback 'on-noop/n '(1 2 3 4 5)) (list 1 2 3 4 5)))
    
    (test-case "call-callback : incorrect arity"
      (check-exn exn:fail:contract:arity? (cut send n1 call-callback 'on-noop/0 (list 1)))
      (check-exn exn:fail:contract:arity? (cut send n1 call-callback 'on-noop/1-2 null)))))

; Helpers ----------------------------------------

(define-class node% html-component% ()
  
  ; -> symbol
  (inherit get-component-id)
  
  ; Fields -------------------------------------
  
  ; (cell boolean)
  (init-cell always-dirty? #f #:accessor #:mutator)
  
  ; (cell (listof html-component<%>))
  (init-cell children null #:children #:accessor #:mutator)
  
  ; Methods ------------------------------------
  
  ; -> boolean
  (define/override (dirty?)
    (or (web-cell-ref always-dirty?-cell)
        (super dirty?)))
  
  ; seed -> xml
  (define/override (render seed)
    (xml (node (@ [id ,(get-component-id)]) 
               ,(super render seed))))
  
  ; seed -> js
  (define/augride (get-on-attach seed)
    (js (attach ,(get-component-id))))
  
  ; seed -> js
  (define/override (get-on-render seed)
    (js (render ,(get-component-id))))
  
  ; seed -> js
  (define/augride (get-on-detach seed)
    (js (detach ,(get-component-id))))
  
  (define/public #:callback (on-noop/0)
    null)
  
  (define/public #:callback (on-noop/1 arg)
    (list arg))
  
  (define/public #:callback (on-noop/1-2 arg1 [arg2 #f])
    (list arg1 arg2))
  
  (define/public #:callback (on-noop/n . args)
    args))

(define n7 (new node% [component-id 'n7]))
(define n6 (new node% [component-id 'n6]))
(define n5 (new node% [component-id 'n5] [children (list n6 n7)]))
(define n4 (new node% [component-id 'n4]))
(define n3 (new node% [component-id 'n3]))
(define n2 (new node% [component-id 'n2] [children (list n3 n4)]))
(define n1 (new node% [component-id 'n1] [children (list n2 n5)]))

; Provide statements -----------------------------

(provide html-component-tests)
