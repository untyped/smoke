#lang scheme/base

(require scheme/contract
         srfi/13
         (planet untyped/mirrors:2/xml/xml)
         "notification-internal.ss"
         "session-cell.ss")

; Session cell -----------------------------------

; (session-cell (listof xml))
(define notifications-cell (make-session-cell null))

; -> (listof notification)
(define (notifications-ref)
  (session-cell-ref notifications-cell))

; (listof notification) -> void
(define (notifications-set! notifications)
  (session-cell-set! notifications-cell notifications))

; [boolean] -> void
(define (notifications-reset! [reset-sticky? #f])
  (if reset-sticky?
      (session-cell-unset! notifications-cell)
      (notifications-set! (filter notification-sticky? (notifications-ref)))))

; xml+quotable [boolean] -> notification
(define (notifications-add! msg [sticky? #f])
  (let ([notification (create-notification (xml-quote msg) sticky?)])
    (notifications-set! (append (notifications-ref) (list notification)))
    notification))

; xml+quotable -> notification
(define (notifications-add-sticky! msg)
  (notifications-add! (xml-quote msg) #t))

; (U notification symbol) -> void
(define (notifications-remove! notification+id)
  (notifications-set! (filter (if (notification? notification+id)
                                  (lambda (notification)
                                    (not (eq? notification notification+id)))
                                  (lambda (notification)
                                    (not (eq? (notification-id notification) notification+id))))
                              (notifications-ref))))

; Provide statements -----------------------------

(provide (except-out (all-from-out "notification-internal.ss") make-notification)
         (rename-out [create-notification make-notification]))

(provide/contract
 [notifications-cell        session-cell?]
 [notifications-ref         (-> (listof notification?))]
 [notifications-set!        (-> (listof notification?) void?)]
 [notifications-reset!      (->* () (boolean?) void?)]
 [notifications-add!        (->* (xml+quotable?) (boolean?) notification?)]
 [notifications-add-sticky! (-> xml+quotable? notification?)]
 [notifications-remove!     (-> (or/c notification? symbol?) void?)])