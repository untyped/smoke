#lang web-server

(require net/url
         "../test-base.ss"
         "continuation-url.ss")

; Helpers ----------------------------------------

(define (test:path->url path)
  (make-url #f #f #f #f #t path null #f))

; Tests ------------------------------------------

(define continuation-url-tests
  (test-suite "continuation-url.ss"
    
    (test-case "url-path-base"
      (check-equal? (url->string (test:path->url (url-path-base (url-path (string->url "/"))))) "/")
      (check-equal? (url->string (test:path->url (url-path-base (url-path (string->url "/abc"))))) "/abc")
      (check-equal? (url->string (test:path->url (url-path-base (url-path (string->url "/abc/_/def"))))) "/abc"))
    
    (test-case "url-path-extension"
      (check-false (url-path-extension (url-path (string->url "/"))))
      (check-false (url-path-extension (url-path (string->url "/abc"))))
      (check-equal? (url->string (test:path->url (url-path-extension (url-path (string->url "/abc/_/def"))))) "/def")
      (check-equal? (url->string (test:path->url (url-path-extension (url-path (string->url "/abc/_/def/1*2*3"))))) "/def/1*2*3")
      (check-equal? (url->string (test:path->url (url-path-extension (url-path (string->url "/abc/_/def;1*2*3"))))) "/def"))
    
    (test-case "url->initial-url"
      (check-equal? (url->string (url->initial-url (string->url "/_/;((%22k%22%20.%20%22(1%202%203)%22))"))) "/")
      (check-equal? (url->string (url->initial-url (string->url "_/;((%22k%22%20.%20%22(1%202%203)%22))"))) "")
      (check-equal? (url->string (url->initial-url (string->url "/a/_/;((%22k%22%20.%20%22(1%202%203)%22))"))) "/a")
      (check-equal? (url->string (url->initial-url (string->url "a/_/;((%22k%22%20.%20%22(1%202%203)%22))"))) "a")
      (check-equal? (url->string (url->initial-url (string->url "/a//_/;((%22k%22%20.%20%22(1%202%203)%22))"))) "/a/")
      (check-equal? (url->string (url->initial-url (string->url "/a///_/;((%22k%22%20.%20%22(1%202%203)%22))"))) "/a//")
      (check-equal? (url->string (url->initial-url (string->url "/a/_/b/c;((%22k%22%20.%20%22(1%202%203)%22))"))) "/a")
      (check-equal? (url->string (url->initial-url (string->url "a/_/b/c;((%22k%22%20.%20%22(1%202%203)%22))"))) "a")
      (check-equal? (url->string (url->initial-url (string->url "/a/b/c/_/d/e;((%22k%22%20.%20%22(1%202%203)%22))"))) "/a/b/c")
      (check-equal? (url->string (url->initial-url (string->url "a/b/c/_/d/e;((%22k%22%20.%20%22(1%202%203)%22))"))) "a/b/c")
      (check-equal? (url->string (url->initial-url (string->url "/a/b/c/d/e;((%22k%22%20.%20%22(1%202%203)%22))"))) "/a/b/c/d/e")
      (check-equal? (url->string (url->initial-url (string->url "a/b/c/d/e;((%22k%22%20.%20%22(1%202%203)%22))"))) "a/b/c/d/e"))
    
    (test-case "url->continuation-url"
      (let ([code (list 1 2 3)])
        (check-equal? (url->string (url->continuation-url (string->url "/") code)) "/;((%22k%22%20.%20%22(1%202%203)%22))")
        (check-equal? (url->string (url->continuation-url (string->url "") code)) ";((%22k%22%20.%20%22(1%202%203)%22))")
        (check-equal? (url->string (url->continuation-url (string->url "/a") code)) "/a;((%22k%22%20.%20%22(1%202%203)%22))")
        (check-equal? (url->string (url->continuation-url (string->url "/a/") code)) "/a/;((%22k%22%20.%20%22(1%202%203)%22))")
        (check-equal? (url->string (url->continuation-url (string->url "/a//") code)) "/a//;((%22k%22%20.%20%22(1%202%203)%22))")
        (check-equal? (url->string (url->continuation-url (string->url "/a/_/b/c") code)) "/a/_/b/c;((%22k%22%20.%20%22(1%202%203)%22))")
        (check-equal? (url->string (url->continuation-url (string->url "/a//b/c") code)) "/a//b/c;((%22k%22%20.%20%22(1%202%203)%22))")))
    
    (test-case "continuation-url->codes"
      (check-equal? (continuation-url->codes (string->url "/a/b/c/_/d/e/f;((%22k%22%20.%20%22(1%202%203)%22))")) (list 1 2 3))
      (check-equal? (continuation-url->codes (string->url "/a/b/c/d/e/f;((%22k%22%20.%20%22(1%202%203)%22))")) (list 1 2 3)))))

; Provide statements -----------------------------

(provide continuation-url-tests)

