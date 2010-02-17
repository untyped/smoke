#lang scheme

(require "../base.ss")

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

; Initialize the web frame on an initial or callback request.
; Either restore the frame from disk, or create a new blank frame.
; -> void
(define (init-web-frame)
  (let* ([request      (current-request)]
         [url          (request-uri request)]
         [callback?    (callback-url? url)]
         [saved-serial (and callback? (generate-web-frame-serial request))]
         [saved-frame  (with-handlers ([exn? (lambda _ #f)])
                         (and callback? (load-web-frame saved-serial)))]
         [initialized? (cond [(and callback? saved-frame)
                              (update-web-frame! saved-frame)
                              #t]
                             [(not callback?)
                              (clear-web-frame!)
                              #t]
                             [else #f])])
    (if initialized?
        (let ([cb-serial (generate-callback-serial)])
          (current-callback-serial-set! cb-serial)
          (current-web-frame-serial-set! (generate-web-frame-serial request cb-serial)))
        (raise-exn exn:fail:smoke:callback "no data for callback" saved-serial))))

; Restore the web frame on a continuation request.
; frame -> void
(define (restore-web-frame frame)
  (update-web-frame! frame)
  (current-callback-serial-set!  (generate-callback-serial))
  (current-web-frame-serial-set! (generate-web-frame-serial
                                  (current-request)
                                  (current-callback-serial))))

; Saves the current web frame under the serial stored in the frame.
; [string] -> frame
(define (save-web-frame [serial (current-web-frame-serial)])
  (printf "saving  frame ~a~n" serial)
  (let ([frame (capture-web-frame)])
    (with-output-to-file (serial->path serial)
      (lambda ()
        (write (serialize frame)))
      #:exists 'replace)
    frame))

; Helpers -----------------------------------------

; string -> path
(define (serial->path serial)
  (build-path (current-directory)
              (format "~a.webframe" serial)))

; Loads a web frame using the supplied serial.
; string -> (U web-frame #f)
(define (load-web-frame serial)
  (if (file-exists? (serial->path serial))
      (begin (printf "loading frame ~a~n" serial)
             (with-input-from-file (serial->path serial)
               (lambda ()
                 (deserialize (read)))))
      (begin (printf "no load frame ~a~n" serial)
             #f)))

; Provides ---------------------------------------

(provide/contract
 [init-web-frame    (-> void?)]
 [save-web-frame    (->* () (string?) web-frame?)]
 [restore-web-frame (-> web-frame? void?)])
