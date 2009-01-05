#lang scheme/base

(require (only-in srfi/1 list-index)
         "submit-button.ss"
         "../../lib-base.ss"
         "html-element.ss")

; Components -------------------------------------


(define wizard-step<%>
  (interface ()
    render-upper-progress-bar? ; -> boolean
    render-lower-progress-bar? ; -> boolean
    get-label                  ; -> xml
    start-wizard-step          ; any -> void
    end-wizard-step))          ; -> any



(define wizard-step%
  (class/cells html-element% (wizard-step<%>)
    
    (inherit core-html-attributes)
    
    ; Fields -------------------------------------
    
    ; (cell xml)
    (init-cell label
               #:accessor #:mutator)
    
    ; (cell html-component<%>)
    (init-cell content
               #:child #:accessor #:mutator)
    
    ; (listof symbol)
    (init [classes null])    
    
    ; Constructor --------------------------------
    
    (super-new [classes (cons 'smoke-wizard-step classes)])
    
    ; Methods ------------------------------------
    
    ; -> boolean
    (define/public (render-upper-progress-bar?)
      (error "(render-upper-progress-bar?) must be overridden"))
    
    ; -> boolean
    (define/public (render-lower-progress-bar?)
      (error "(render-lower-progress-bar?) must be overridden"))
    
    ; any -> void
    (define/public (start-wizard-step data)
      (error "(start-wizard-step) must be overridden"))
    
    ; -> (U any #f) - #f is STOP!
    (define/public (end-wizard-step)
      (error "(end-wizard-step) must be overridden"))
    
    ; seed -> xml
    (define/override (render seed)
      (xml (div (@ ,@(core-html-attributes seed))
                ,(send (get-content) render seed))))))




(define wizard%
  (class/cells html-element% ()
    
    (inherit get-id
             core-html-attributes)
    
    ; Fields -------------------------------------    
    
    ; (cell (listof wizard-step%))
    (init-cell wizard-steps
               #:accessor #:mutator)
    
    ; (cell (U wizard-step% #f))
    (init-cell [current-wizard-step (car wizard-steps)]
               #:child #:accessor #:mutator)
    
    ; wizard-step that preceded current wizard step
    ; (cell (U wizard-step% #f))
    (init-cell [directed-from #f] #:accessor #:mutator)
    
    ; (listof symbol)
    (init [classes null])
    
    ; submit-button%
    (field [next-field (new submit-button% 
                            [id 'next-button]
                            [label "Next >>"]
                            [action (callback on-next)])]
           #:child)
    
    
    #;(field [previous-field (new submit-button% 
                                  [id 'back-button]
                                  [label "<< Back"]
                                  [action (callback on-back)])]
             #:child)
    
    ; Constructor --------------------------------
    
    (super-new [classes (cons 'smoke-wizard classes)])
    
    ; Methods ------------------------------------
    
    ; symbol -> (U wizard-step% #f)
    (define/public (get-wizard-step id)
      (ormap (lambda (wizard-step)
               (and (eq? (send wizard-step get-component-id) id) wizard-step))
             (get-wizard-steps)))
    
    ; -> (U wizard-step% #f)
    (define/private (get-previous-wizard-step)
      (define (select-step wizard-steps current-step previous-step)
        (cond [(null? wizard-steps)       #f]
              [(equal? (car wizard-steps) 
                       current-step)      previous-step]
              [else                       (select-step (cdr wizard-steps) current-step (car wizard-steps))]))
      (select-step (get-wizard-steps) (get-current-wizard-step) #f))
    
    ; -> (U wizard-step% #f)
    (define/private (get-next-wizard-step)
      (define (select-step wizard-steps this-one? current-step)
        (cond [(null? wizard-steps) #f]
              [this-one?            (car wizard-steps)]
              [else                 (select-step (cdr wizard-steps) (equal? (car wizard-steps) current-step) current-step)]))
      (select-step (get-wizard-steps) #f (get-current-wizard-step)))
    
    ; seed -> xml
    (define/override (render seed)
      ; (listof wizard-step%)
      (define wizard-steps
        (get-wizard-steps))
      ; wizard-step%
      (define current-wizard-step
        (get-current-wizard-step))
      ; xml
      (xml 
       (div 
        (@ ,@(core-html-attributes seed))
        ,(let ([wizard-bar ; (U xml #f)
                (and (not (null? wizard-steps))
                     (xml (table 
                           (@ [class 'labels])
                           (tr 
                            ,(let*-values
                                 ([(steps)            (length (get-wizard-steps))]                                 
                                  [(step-width)       (floor (/ 90 steps))]
                                  [(current-index)    (list-index (cut eq? <> current-wizard-step) wizard-steps)]
                                  [(discard list-xml)                          
                                   (for/fold ([after-current? #f]
                                              [xml-accum      null])
                                             ([wizard-step (get-wizard-steps)]
                                              [i           (in-naturals)])
                                             ; symbol
                                             (define id
                                               (send wizard-step get-component-id))
                                             ; boolean
                                             (define current?
                                               (eq? wizard-step current-wizard-step))
                                             ; symbol
                                             (define class
                                               (cond [(and current?
                                                           (= i (sub1 steps)))   'current-final] 
                                                     [(= i (sub1 steps))         'future-final]
                                                     [current?                   'current]
                                                     [after-current?             'future]
                                                     [(= i (sub1 current-index)) 'past-current]
                                                     [else                       'past]))
                                             ; (U callback #f)
                                             (define onclick
                                               (and (not (or current? after-current?))
                                                    (embed/ajax seed (callback on-select id))))
                                             
                                             ; accumulate (values boolean (listof xml))
                                             (values (or after-current? current?)
                                                     (cons 
                                                      (xml (td (@ [width ,(format "~a%" step-width)] [class ,class])
                                                               (span (@ [class 'wizard-step-number]) ,(format "[~a]" (add1 i)))
                                                               (span (@ [class 'wizard-step-title]) ,(send wizard-step get-label))))
                                                      xml-accum)))])
                               (xml (td (@ [class ,(if (= current-index 0) 'start-current 'start)]) "Go")
                                    ,@(reverse list-xml)
                                    (td (@ [class 'final])
                                        "Ok")))))))])
           (xml 
            ,(opt-xml (and wizard-bar (send current-wizard-step render-upper-progress-bar?))
               ,wizard-bar)
            (div (@ [class 'current-wizard-step])
                 ,(send current-wizard-step render seed))
            (div (@ [class 'wizard-navigation])                        
                 (span (@ ,(opt-xml-attr (not (get-next-wizard-step)) class 'hidden)) 
                       ,(send next-field render seed)))                   
            ,(opt-xml (and wizard-bar (send current-wizard-step render-lower-progress-bar?))
               ,wizard-bar))))))
    
    
    (define/public #:callback (on-next)
      (printf "Next clicked~n")
      (let ([result (send (get-current-wizard-step) end-wizard-step)])
        (when result
          (let ([next-step (get-next-wizard-step)])
            (when next-step
              (begin
                (send next-step start-wizard-step result)
                (set-current-wizard-step! next-step)))))))
    
    #;(define/public #:callback (on-back)
        (let ([result (send (get-current-wizard-step) end-wizard-step)])
          (when result
            (let ([next-step (get-next-wizard-step)])
              (send next-step start-wizard-step result)
              (set-current-wizard-step! next-step)))))
    
    ; integer -> void
    (define/public #:callback (on-select id)
      (set-current-wizard-step! (get-wizard-step id)))))

; Provide statements -----------------------------

(provide wizard-step%
         wizard-step<%>
         wizard%)
