#lang scheme

(require file/md5
         net/url
         scheme/serialize
         web-server/http
         (planet untyped/unlib:3/debug)
         "callback-url.ss"
         "env.ss"
         "request.ss"
         "web-cell.ss")

; Frame paths ------------------------------------

; [string] -> path
(define (serial->path [serial (web-frame-serial)])
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

; -> string
(define (generate-serial)
  (let ([serial-seed2 (current-inexact-milliseconds)])
    (bytes->string/utf-8
     (md5 (string->bytes/utf-8
           (string-append (number->string serial-seed1)
                          (number->string serial-seed2)))))))

; Loading and saving web frames ------------------

; Loads a web frame using the supplied serial.
; string -> (U web-frame #f)
(define (load-web-frame serial)
  (and (file-exists? (serial->path serial))
       (with-input-from-file (serial->path serial)
         (lambda ()
           (deserialize (read))))))

; Saves the current web frame under the serial stored in the frame.
; -> void
(define (save-web-frame!)
  (with-output-to-file (serial->path (web-frame-serial))
    (lambda ()
      (write (serialize (capture-web-frame))))
    #:exists 'replace))

; (_ expr ...)
(define-syntax-rule (with-saved-web-frame expr ...)
  (let* ([request (current-request)]
         [url     (request-uri request)])
    (debug (format "===== ~a request-url ====="
                   (if (ajax-request? request) 'ajax 'full))
           (url->string url))
    ; Create or restore web frame:
    (cond [(and (debug* "callback?" callback-url? url)
                (debug* "load-frame" load-web-frame (request-serial)))
           => (lambda (saved)
                (debug "request-serial" (request-serial))
                (update-web-frame! saved)
                (unless (ajax-request? request)
                  (web-frame-serial-set! (generate-serial))
                  (debug "reset-serial" (web-frame-serial))))]
          [else (clear-web-frame!)
                (web-frame-serial-set! (generate-serial))
                (debug "init-serial" (web-frame-serial))])
    (begin0
      ; Run the body expressions:
      (begin expr ...)
      ; Save the web frame:
      (save-web-frame!)
      (debug "saved-serial" (web-frame-serial)))))

; Provides ---------------------------------------

(provide with-saved-web-frame)
