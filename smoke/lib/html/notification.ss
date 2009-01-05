#lang scheme/base

(require srfi/13
         (planet untyped/unlib:3/symbol)
         "../../lib-base.ss"
         "browser-util.ss"
         "html-element.ss")

; Interfaces -------------------------------------

(define notification-element<%>
  (interface (html-element<%>)
    get-notifications      ; -> (listof xml)
    add-notification!      ; xml -> void
    reset-notifications!   ; -> void
    render-notifications)) ; seed -> xml

; Mixins -----------------------------------------

(define notification-mixin
  (mixin/cells (html-element<%>) (notification-element<%>)
    
    (inherit get-id)
    
    ; Fields -------------------------------------
    
    ; (cell (U (listof xml) #f))
    ;
    ; This web cell stores a list of notifications in all web frames
    ; except the initial frame, where it stores #f. This lets us erase
    ; the notification history after notifications have been displayed.
    (init-cell [notifications #f])
    
    ; Methods ------------------------------------
    
    ; -> (listof notifications)
    (define/public (get-notifications)
      (or (web-cell-ref notifications-cell) null))
    
    ; (listof notifications) -> void
    (define/public (set-notifications! notifications)
      (when notifications
        (web-cell-set! notifications-cell notifications)))
    
    ; -> void
    ; 
    ; Erase all notifications from the web history.
    (define/public (reset-notifications!)
      (let/ec escape
        (for ([frame (in-frames)])
          (if (web-cell-ref notifications-cell frame)
              (web-cell-unset! notifications-cell frame)
              (escape)))))
    
    ; xml -> void
    (define/public (add-notification! notification)
      (set-notifications! (append (get-notifications) (list notification))))
    
    ; -> (listof (U xml (seed -> xml)))
    (define/augment (get-html-requirements)
      (list* rollover-script
             (inner null get-html-requirements)))

    ; seed -> xml
    (define/public (render-notifications seed)
      (define id (get-id)) ; (U symbol #f)
      (define notifications (get-notifications)) ; (listof xml)
      (opt-xml (not (null? notifications))
        (ul (@ [class 'smoke-notifications])
            ,@(for/list ([index (in-naturals)] [notification notifications])
                (let* ([notification-id (format "~a-notification~a" id index)]
                       [dismiss-id      (string-append notification-id "-dismiss")])
                  (xml (div (@ [id ,notification-id] [class "notification"])
                            (img (@ [id    ,dismiss-id]
                                    [class "dismiss rollover"]
                                    [src   "/images/smoke/dismiss.png"]
                                    [title "Dismiss this notification"]
                                    [alt   "Dismiss this notification"]))
                            ,notification)))))))
      
    ; seed -> js
    (define/augride (get-on-attach seed)
      ; (U symbol #f)
      (define id (get-id))
      ; (listof xml)
      (define notifications (get-notifications))
      (reset-notifications!)
      (js ,@(for/list ([index (in-naturals)] [notification notifications])
              (let* ([notification-selector (format "#~a-notification~a" id index)]
                     [dismiss-selector      (string-append notification-selector "-dismiss")])
                (js (!dot ($ ,dismiss-selector)
                          (click (function ()
                                   (!dot ($ ,dismiss-selector) 
                                         (unbind))
                                   (!dot ($ ,notification-selector)
                                         (unbind)
                                         (fadeOut "fast"))))))))))))

; Provide statements -----------------------------

(provide notification-element<%>
         notification-mixin)
