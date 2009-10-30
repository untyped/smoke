#lang scheme/base

(require net/url
         srfi/19
         "../content-base.ss")

; Controllers ------------------------------------

; request -> response
(define-controller (test-session-show)
  (define session (request-session (current-request)))
  (make-html-response
   (xml (html (head (title "Current session"))
              (body (p "Current session")
                    ,(session-html session))))))

; request -> response
(define-controller (test-session-start)
  (start-session)
  (make-html-response
   (xml (html (head (title "Session started"))
              (body (p "Session started")
                    ,(session-html (request-session (current-request))))))))

; request -> response
(define-controller (test-session-end)
  (define session (request-session (current-request)))
  (end-session session)
  (make-html-response
   (xml (html (head (title "Session ended"))
              (body (p "Session ended")
                    ,(session-html session))))))

; request symbol string -> response
(define-controller (test-session-set key val)
  (define session (request-session (current-request)))
  (session-set! session key val)
  (make-html-response
   (xml (html (head (title "Current session"))
              (body (p "Current session")
                    ,(session-html session))))))

; request symbol -> response
(define-controller (test-session-remove key)
  (define session (request-session (current-request)))
  (session-remove! session key)
  (make-html-response
   (xml (html (head (title "Current session"))
              (body (p "Current session")
                    ,(session-html session))))))

; Helpers ----------------------------------------

(define (session-html session)
  (if session
      (xml (dl (dt "Cookie ID") 
               (dd (@ [id "cookie-id"])
                   ,(session-cookie-id session))
               (dt "Issued")
               (dd (@ [id "issued"])
                   ,(date->string (time-utc->date (session-issued session)) "~Y-~m-~d ~H:~M:~S"))
               (dt "Accessed")
               (dd (dd (@ [id "accessed"])
                       ,(date->string (time-utc->date (session-accessed session)) "~Y-~m-~d ~H:~M:~S")))
               (dt "Data")
               (dd (table (@ [id "hash"])
                          (thead (tr (th "Key")
                                     (th "Val")))
                          (tbody ,@(for/list ([(key val) (in-hash (session-hash session))])
                                     (xml (tr (th ,key)
                                              (td (@ [id ,key]) (pre ,(format "~s" val)))))))))))
      (xml (p "No session"))))
