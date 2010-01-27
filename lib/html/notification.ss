#lang web-server

(require srfi/13
         (planet untyped/unlib:3/symbol)
         "../../lib-base.ss"
         "../../web-server/session-cell.ss"
         "browser-util.ss"
         "html-element.ss"
         "jquery-ui-util.ss")

; Classes ----------------------------------------

(define notification-pane%
  (class/cells html-element% ()
    
    (inherit get-id
             get-classes
             core-html-attributes)
    
    ; (cell (listof symbol))
    (cell visible-notifications null #:accessor #:mutator)
    
    ; Constructor --------------------------------
    
    (init [classes null])
    
    (super-new [classes (list* 'smoke-notifications 'ui-widget classes)])
    
    ; Methods ------------------------------------
        
    ; seed -> xml
    (define/override (render seed)
      (let* ([id            (get-id)]                 ; (U symbol #f)
             [notifications (notifications-ref)]      ; (listof xml)
             [classes       (if (null? notifications) ; (listof (U symbol string))
                                (cons 'empty (get-classes))
                                (get-classes))])
        (set-visible-notifications! (map notification-id notifications))
        (xml (div (@ ,@(core-html-attributes seed #:classes classes))
                  ,(opt-xml (not (null? notifications))
                     ,@(for/list ([notification (in-list notifications)])
                         (let* ([id              (notification-id notification)]
                                [notification-id (format "notification-~a" id)]
                                [dismiss-id      (string-append notification-id "-dismiss")])
                           (xml (div (@ [id    ,notification-id]
                                        [class ,(if (notification-sticky? notification)
                                                    "notification ui-state-highlight ui-corner-all"
                                                    "notification ui-state-highlight ui-corner-all")])
                                     ,(opt-xml (notification-sticky? notification)
                                        (span (@ [class "ui-icon notification-sticky"]
                                                 [title "Sticky notification"]
                                                 [alt   "Sticky notification"])))
                                     (span (@ [id    ,dismiss-id]
                                              [class "ui-icon notification-dismiss"]
                                              [title "Dismiss this notification"]
                                              [alt   "Dismiss this notification"]))
                                     (div (@ [class "notification-content"])
                                          ,(notification-xml notification)))))))))))
    
    ; -> boolean
    (define/override (dirty?)
      (or (super dirty?)
          (not (equal? (get-visible-notifications)
                       (map notification-id (notifications-ref))))))
    
    ; seed -> js
    (define/augride (get-on-attach seed)
      ; (U symbol #f)
      (define id (get-id))
      ; (listof xml)
      (define notifications (notifications-ref))
      (notifications-reset!)
      (js ,@(for/list ([notification (in-list notifications)])
              (let* ([id                    (notification-id notification)]
                     [notification-selector (format "#notification-~a" id)]
                     [dismiss-selector      (string-append notification-selector "-dismiss")])
                (js (!dot ($ ,dismiss-selector)
                          (hover (function () (!dot ($ this) (addClass "ui-state-hover")))
                                 (function () (!dot ($ this) (removeClass "ui-state-hover"))))
                          (click (function ()
                                   (!dot ($ this) (unbind))
                                   (!dot ($ this) (parent) (unbind)
                                         (fadeOut "fast"
                                                  (function ()
                                                    (!dot ($ ,notification-selector)
                                                          (remove))
                                                    ,(opt-js (notification-sticky? notification)
                                                       ,(embed/ajax seed (callback on-dismiss id))))))))))))))
    
    ; symbol -> void
    (define/public #:callback (on-dismiss id)
      (notifications-remove! id))))

; Provide statements -----------------------------

(provide notification-pane%)
