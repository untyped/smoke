#lang scheme/base

(require srfi/13
         (planet untyped/unlib:3/symbol)
         "../../lib-base.ss"
         "../../web-server/session-cell.ss"
         "browser-util.ss"
         "html-element.ss"
         "notification-internal.ss")

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

; xml [boolean] -> notification
(define (notifications-add! xml [sticky? #f])
  (let ([notification (create-notification xml sticky?)])
    (notifications-set! (append (notifications-ref) (list notification)))
    notification))

; (U notification symbol) -> void
(define (notifications-remove! notification+id)
  (notifications-set! (filter (if (notification? notification+id)
                                  (lambda (notification)
                                    (not (eq? notification notification+id)))
                                  (lambda (notification)
                                    (not (eq? (notification-id notification) notification+id))))
                              (notifications-ref))))

; Interfaces -------------------------------------

(define notification-element<%>
  (interface (html-element<%>)
    get-notifications      ; -> (listof notification)
    add-notification!      ; xml [boolean] -> notification
    remove-notification!   ; (U notification symbol) -> void
    reset-notifications!   ; [boolean] -> void
    render-notifications)) ; seed -> xml

; Mixins -----------------------------------------

(define notification-mixin
  (mixin/cells (html-element<%>) (notification-element<%>)
    
    (inherit get-id)
    
    ; Methods ------------------------------------
    
    ; -> (listof notifications)
    (define/public (get-notifications)
      (notifications-ref))
    
    ; (listof notifications) -> void
    (define/public (set-notifications! notifications)
      (when notifications
        (notifications-set! notifications)))
    
    ; [boolean] -> void
    (define/public (reset-notifications! [sticky? #f])
      (notifications-reset! sticky?))
    
    ; xml [boolean] -> notification
    (define/public (add-notification! xml [sticky? #f])
      (notifications-add! xml sticky?))
    
    ; (U notification symbol) -> void
    (define/public (remove-notification! notification+id)
      (notifications-remove! notification+id))
    
    ; -> (listof (U xml (seed -> xml)))
    (define/augment (get-html-requirements)
      (list* rollover-script (inner null get-html-requirements)))
    
    ; seed -> xml
    (define/public (render-notifications seed)
      (define id (get-id)) ; (U symbol #f)
      (define notifications (get-notifications)) ; (listof xml)
      (opt-xml (not (null? notifications))
        (ul (@ [class 'smoke-notifications])
            ,@(for/list ([notification notifications])
                (let* ([id              (notification-id notification)]
                       [notification-id (format "notification-~a" id)]
                       [dismiss-id      (string-append notification-id "-dismiss")])
                  (xml (div (@ [id    ,notification-id]
                               [class ,(if (notification-sticky? notification)
                                           "notification sticky"
                                           "notification")])
                            ,(opt-xml (notification-sticky? notification)
                               (img (@ [class "sticky"]
                                       [src   "/images/smoke/sticky.png"]
                                       [title "Sticky notification"]
                                       [alt   "Sticky notification"])))
                            (img (@ [id    ,dismiss-id]
                                    [class "dismiss rollover"]
                                    [src   "/images/smoke/dismiss.png"]
                                    [title "Dismiss this notification"]
                                    [alt   "Dismiss this notification"]))
                            ,(notification-xml notification))))))))
    
    ; seed -> js
    (define/augride (get-on-attach seed)
      ; (U symbol #f)
      (define id (get-id))
      ; (listof xml)
      (define notifications (get-notifications))
      (reset-notifications!)
      (js ,@(for/list ([notification notifications])
              (let* ([id                    (notification-id notification)]
                     [notification-selector (format "#notification-~a" id)]
                     [dismiss-selector      (string-append notification-selector "-dismiss")])
                (js (!dot ($ ,dismiss-selector)
                          (click (function ()
                                   ,(if (notification-sticky? notification)
                                        (embed/ajax seed (callback on-dismiss id))
                                        (js))
                                   (!dot ($ ,dismiss-selector) 
                                         (unbind))
                                   (!dot ($ ,notification-selector)
                                         (unbind)
                                         (fadeOut "fast")
                                         (remove))))))))))
    
    ; symbol -> void
    (define/public #:callback (on-dismiss id)
      (printf "on-dismiss ~a~n" id)
      (remove-notification! id))))

; Provide statements -----------------------------

(provide (except-out (all-from-out "notification-internal.ss") make-notification)
         (rename-out [create-notification make-notification])
         notification-element<%>
         notification-mixin)

(provide/contract
 [notifications-cell    session-cell?]
 [notifications-ref     (-> (listof notification?))]
 [notifications-set!    (-> (listof notification?) void?)]
 [notifications-reset!  (->* () (boolean?) void?)]
 [notifications-add!    (->* (xml?) (boolean?) notification?)]
 [notifications-remove! (-> (or/c notification? symbol?) void?)])
