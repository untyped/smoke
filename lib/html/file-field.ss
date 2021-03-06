#lang scheme/base

(require "../../lib-base.ss")

(require (only-in srfi/13 string-index-right string-drop)
         (only-in srfi/14 char-set)
         "form-element.ss")

; Uploaded files typically take up a lot of memory, so file-field% does not keep a copy of the uploaded data:
;
;   - get-value delegates to the current request;
;   - set-value! raises an exception;
;   - value-changed? always returns #f;
;   - on-request does nothing.
;
; Also note that files cannot be uploaded via AJAX without special hacks involving IFrames or the like.
; file-field% doesn't do any of this stuff - it will never receive content as part of an AJAX request.

(define file-field%
  (class/cells form-element% ()
    
    (inherit get-id
             core-html-attributes)

    ; Fields -------------------------------------
    
    ; (cell (U natural #f))
    (init-cell size #f #:accessor #:mutator)

    ; Constructor --------------------------------
    
    ; (listof symbol)
    (init [classes null])
    
    (super-new [classes (cons 'smoke-file-field classes)])
    
    ; Public methods -----------------------------
    
    ; -> (U bytes #f)
    (define/override (get-value)
      (request-upload-content-ref (current-request) (get-id)))
    
    ; bytes -> void | exn:fail
    (define/override (set-value! value)
      (error "Cannot set the value of a file-field%."))
    
    ; -> (U string #f)
    (define/public (get-full-filename)
      (request-upload-filename-ref (current-request) (get-id)))
    
    ; -> (U string #f)
    (define/public (get-filename)
      (let ([filename (get-full-filename)])
        (and filename
             (let ([slash-pos (string-index-right filename (char-set #\\ #\/))])
               (if slash-pos
                   (string-drop filename (add1 slash-pos))
                   filename)))))
    
    ; -> boolean
    (define/override (value-changed?)
      #f)
    
    ; seed -> xml
    (define/override (render seed)
      (define size (get-size))
      (xml (input (@ ,@(core-html-attributes seed)
                     [type "file"]
                     ,@(if size (xml-attrs [size ,size]) null)))))))

; Provide statements -----------------------------

(provide file-field%)
