#lang scheme/base

(require net/url
         scheme/contract
         scheme/list
         scheme/match
         web-server/private/url-param
         web-server/private/util
         (planet untyped/unlib:3/debug)
         (planet untyped/unlib:3/list)
         (planet untyped/unlib:3/number))

; Variables --------------------------------------

; path/param
(define SEPARATOR 
  (make-path/param "_" null))

; Procedures -------------------------------------

; url -> url
(define (url->initial-url url)
  (make-url (url-scheme url)
            (url-user url)
            (url-host url)
            (url-port url)
            (url-path-absolute? url)
            (let ([base (url-path-base (url-path url))])
              (if (null? base)
                  (list (make-path/param "" null))
                  (remove-params base)))
            (url-query url)
            (url-fragment url)))

; (list number number number) url -> url
(define (url->continuation-url url codes)
  (insert-param (remove-url-params url) "k" (write/string codes)))

; url -> (U (list natural natural natural) #f)
(define (continuation-url->codes url)
  ; (U string #f)
  (let ([str (extract-param url "k")])
    ; (U (list natural natural natural) #f)
    (and str (read/string str))))

; (listof path/param) -> (listof path/param)
(define (url-path-base path)
  (let loop ([rest path] [accum null])
    (match rest
      [(list) (reverse accum)]
      [(list-rest curr rest)
       (if (separator? curr)
           (remove-params (reverse accum))
           (loop rest (cons curr accum)))])))

; (listof path/param) -> (U (listof path/param) #f)
(define (url-path-extension path)
  (let loop ([rest path])
    (match rest
      [(list) #f]
      [(list-rest curr rest)
       (if (separator? curr)
           (remove-params rest)
           (loop rest))])))

; Helpers ----------------------------------------

; path/param -> boolean
(define (separator? path/param)
  (equal? (path/param-path path/param)
          (path/param-path SEPARATOR)))

; (listof path/param) string -> (listof path/param)
(define (path->continuation-path path code)
  (let ([path (if (and (not (null? path))
                       (equal? (path/param-path (last path)) ""))
                  (reverse (drop (reverse path) 1))
                  path)])
    (if (url-path-extension path)
        (append path (list (make-path/param code null)))
        (append path (list SEPARATOR (make-path/param code null))))))

; (listof path/param) 
(define (continuation-path->code path)
  (cond [(url-path-extension path)
         => (lambda (extension)
              (and (not (null? extension))
                   (path/param-path (car (reverse extension)))))]
        [else #f]))

; url -> url
(define (remove-url-params url)
  (make-url (url-scheme url)
            (url-user url)
            (url-host url)
            (url-port url)
            (url-path-absolute? url)
            (remove-params (url-path url))
            (url-query url)
            (url-fragment url)))

; (listof path/param) -> (listof path/param)
(define (remove-params path/params)
  (for/list ([curr (in-list path/params)])
    (make-path/param (path/param-path curr) null)))

; Provide statements -----------------------------

(provide/contract
 [url->initial-url        (-> url? url?)]
 [url->continuation-url   (-> url? (list/c natural? natural? natural?) url?)]
 [continuation-url->codes (-> url? (or/c (list/c natural? natural? natural?) false/c))]
 [url-path-base           (-> (listof path/param?) (listof path/param?))]
 [url-path-extension      (-> (listof path/param?) (or/c (listof path/param?) #f))])

