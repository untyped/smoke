#lang scheme

(require file/md5
         net/url
         scheme/serialize
         web-server/http
         (planet untyped/mirrors:2)
         (planet untyped/unlib:3/debug)
         "callback-url.ss"
         "env.ss"
         "request.ss"
         "serial.ss"
         "web-cell.ss")

; Frame paths ------------------------------------

; string -> path
(define (serial->path serial)
  (build-path (current-directory)
              (format "~a.webframe" serial)))

; Generating serials -----------------------------

; Serials are generated from two timestamps:
;   - the start time of the application;
;   - the generation time of the serial.
; This ensures they're different for each run of the application.

; natural
(define serial-seed1
  (current-inexact-milliseconds))

; Loading and saving web frames ------------------

; Loads a web frame using the supplied serial.
; string -> (U web-frame #f)
(define (load-web-frame serial)
  (and (file-exists? (serial->path serial))
       (with-input-from-file (serial->path serial)
         (lambda ()
           (deserialize (read))))))

; Saves the current web frame under the serial stored in the frame.
; string -> void
(define (save-web-frame! serial)
  (with-output-to-file (serial->path serial)
    (lambda ()
      (write (serialize (capture-web-frame))))
    #:exists 'replace))

; (_ expr ...)
(define-syntax-rule (with-saved-web-frame expr ...)
  (let* ([request      (current-request)]
         [url          (request-uri request)]
         [callback?    (callback-url? url)]
         [saved-frame  (with-handlers ([exn? (lambda _ #f)])
                         (and callback? (load-web-frame (generate-web-frame-serial (current-request)))))]
         [initialized? (cond [(and callback? saved-frame)
                              (update-web-frame! saved-frame)
                              #t]
                             [(not callback?)
                              (clear-web-frame!)
                              #t]
                             [else #f])])
    (if initialized?
        (let* ([cb-serial (generate-callback-serial)]
               [wf-serial (generate-web-frame-serial request cb-serial)])
          (begin0
            ; Run the body expressions:
            (parameterize ([current-callback-serial cb-serial])
              (begin expr ...))
            ; Save the web frame:
            (save-web-frame! wf-serial)))
        (make-redirect-response (url->initial url)))))

; Provides ---------------------------------------

(provide with-saved-web-frame)
