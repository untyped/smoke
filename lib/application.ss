#lang scheme

(require "../lib-base.ss")

(define application%
  (class/cells object/cells% (application<%>)
    
    (init-field page)
    
    (define/public (dispatch)
      (let ([request (current-request)])
        (send page respond)))))

; Provides ---------------------------------------

(provide application%)