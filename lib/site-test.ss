#lang scheme

(require "../test-base.ss")

(require net/uri-codec
         srfi/19
         (mirrors-in)
         (unlib-in time)
         "main.ss")

(require/expose web-server/dispatchers/dispatch
  (exn:dispatcher?))

; Test data --------------------------------------

(define-site math site%
  ([("/divide/"    (integer-arg)           "/" (integer-arg))           divide-numbers]
   [("/add/"       (integer-arg)           "/" (integer-arg))           add-numbers]
   [("/subtract/"  (integer-arg)           "/" (integer-arg))           subtract-numbers]
   [("/and/"       (boolean-arg)           "/" (boolean-arg))           and-booleans]
   [("/after/"     (time-utc-arg "~Y~m~d") "/" (time-utc-arg "~Y~m~d")) time-after]))

(define-page divide-numbers html-page% ()
    
  ; integer integer -> real
  (define/override (dispatch num den)
    (/ num den))
  
  ; integer integer -> boolean
  (define/override (access-allowed? num den)
    (not (zero? den))))

(define-page add-numbers html-page% ()
  
  ; request integer integer -> integer
  (define/override (dispatch first second)
    (+ first second)))

; Leave subtract-numbers undefined.

(define-page and-booleans html-page% ()
  
  ; boolean boolean -> boolean
  (define/override (dispatch first second)
    (if (and (boolean? first) (boolean? second))
        (and first second)
        (raise-type-error #f "booleans" (list first second)))))

(define-page time-after
  
  ; time-utc time-utc -> boolean
  (define/override (dispatch first second)
    (if (and (time-utc? first) (time-utc? second))
        (time>? first second)
        (raise-type-error #f "time-utcs" (list first second)))))

; Tests ------------------------------------------

(define/provide-test-suite codec-tests
  
  (test-equal? "clean-request-url"
    (clean-request-url (test-request (format "/~a/~a/" (uri-encode "a/b") (uri-encode "c?d"))))
    (format "/~a/~a/" (uri-encode "a/b") (uri-encode "c?d")))
  
  (test-case "site-dispatch : divide-numbers"
    (check-equal? (site-dispatch math (test-request "/divide/8/2")) 4)
    (check-equal? (site-dispatch math (test-request "/divide/8/4")) 2))
  
  (test-case "site-dispatch : add-numbers"
    (check-equal? (site-dispatch math (test-request "/add/1/2"))    3))
  
  (test-case "site-dispatch : controller undefined"
    (check-pred response/full? (site-dispatch math (test-request "/subtract/1/2")))
    (parameterize ([default-controller-undefined-responder
                     (lambda (controller request . args)
                       (cons (controller-id controller) args))])
      (check-equal? (site-dispatch math (test-request "/subtract/1/2"))
                    '(subtract-numbers 1 2))))
  
  (test-case "site-dispatch : access denied"
    (check-pred response/full? (site-dispatch math (test-request "/divide/8/0")))
    (parameterize ([default-access-denied-responder
                     (lambda (controller request . args)
                       (cons (controller-id controller) args))])
      (check-equal? (site-dispatch math (test-request "/divide/8/0"))
                    '(divide-numbers 8 0))))
  
  (test-case "site-dispatch : controller not found"
    ; We can't use check-exn because exn:dispatcher isn't actually an exn:
    (check-true (with-handlers ([exn:dispatcher? (lambda _ #t)])
                  (site-dispatch math (test-request "/undefined"))
                  #f)))
  
  (test-case "site-dispatch : anchor / query string / url-params"
    (check-equal? (site-dispatch math (test-request "/divide/8/2#anchor"))    4)
    (check-equal? (site-dispatch math (test-request "/divide/8/4;((a . b))")) 2)
    (check-equal? (site-dispatch math (test-request "/divide/8/8?a=b&c=d"))   1))
  
  (test-case "controller-url : divide-numbers"
    (check-equal? (controller-url divide-numbers 8 2) "/divide/8/2")
    (check-equal? (controller-url divide-numbers 8 4) "/divide/8/4"))
  
  (test-case "controller-url : add-numbers"
    (check-equal? (controller-url add-numbers 1 2)    "/add/1/2"))
  
  (test-case "controller-access? : divide-numbers"
    (check-true  (controller-access? divide-numbers (test-request "foo") 8 2))
    (check-false (controller-access? divide-numbers (test-request "foo") 8 0)))
  
  (test-case "controller-link : no arguments"
    (let* ([link-ref (cut controller-link divide-numbers (test-request "foo") 8 4)]
           [mirrors  (link-ref)]
           [sexp     (parameterize ([default-link-format 'sexp]) (link-ref))]
           [sexps    (parameterize ([default-link-format 'sexps]) (link-ref))])
      (check-pred xml? mirrors)
      (check-equal? (xml->string mirrors) "<a href=\"/divide/8/4\">/divide/8/4</a>")
      (check-equal? sexp  '(a ([href "/divide/8/4"]) "/divide/8/4"))
      (check-equal? sexps '((a ([href "/divide/8/4"]) "/divide/8/4")))))
  
  (test-case "controller-link : all arguments"
    (let* ([link-ref (lambda (body)
                       (controller-link
                        divide-numbers
                        (test-request "foo") 8 4
                        #:id    'id
                        #:class 'class
                        #:title "title"
                        #:body  body))]
           [mirrors  (link-ref "body")]
           [sexp     (parameterize ([default-link-format 'sexp]) (link-ref "body"))]
           [sexps    (parameterize ([default-link-format 'sexps]) (link-ref '("body")))])
      (check-pred xml? mirrors)
      (check-equal? (xml->string mirrors) "<a href=\"/divide/8/4\" id=\"id\" class=\"class\" title=\"title\">body</a>")
      (check-equal? sexp  '(a ([href "/divide/8/4"] [id "id"] [class "class"] [title "title"]) "body"))
      (check-equal? sexps '((a ([href "/divide/8/4"] [id "id"] [class "class"] [title "title"]) "body")))))
  
  (test-case "controller-link : no access : hide"
    (let* ([link-ref (cut controller-link divide-numbers (test-request "foo") 8 0 #:else 'hide)]
           [mirrors  (link-ref)]
           [sexp     (parameterize ([default-link-format 'sexp]) (link-ref))]
           [sexps    (parameterize ([default-link-format 'sexps]) (link-ref))])
      (check-pred xml? mirrors)
      (check-pred xml-empty? mirrors)
      (check-equal? sexp  '(span))
      (check-equal? sexps null)))
  
  (test-case "controller-link : no access : span"
    (let* ([link-ref (cut controller-link divide-numbers (test-request "foo") 8 0 #:else 'span #:id 'id #:class 'class #:title "title")]
           [mirrors  (link-ref)]
           [sexp     (parameterize ([default-link-format 'sexp]) (link-ref))]
           [sexps    (parameterize ([default-link-format 'sexps]) (link-ref))])
      (check-pred xml? mirrors)
      (check-equal? (xml->string mirrors) "<span id=\"id\" class=\"no-access-link class\" title=\"title\">/divide/8/0</span>")
      (check-equal? sexp  '(span ([id "id"] [class "no-access-link class"] [title "title"]) "/divide/8/0"))
      (check-equal? sexps '((span ([id "id"] [class "no-access-link class"] [title "title"]) "/divide/8/0")))))
  
  (test-case "controller-link : no access : body"
    (let* ([link-ref (cut controller-link divide-numbers (test-request "foo") 8 0 #:else 'body)]
           [mirrors  (link-ref)]
           [sexp     (parameterize ([default-link-format 'sexp]) (link-ref))]
           [sexps    (parameterize ([default-link-format 'sexps]) (link-ref))])
      (check-pred xml? mirrors)
      (check-equal? (xml->string mirrors) "/divide/8/0")
      (check-equal? sexp  "/divide/8/0")
      (check-equal? sexps '("/divide/8/0"))))
  
  (test-case "default-controller-wrapper"
    (parameterize ([default-controller-wrapper
                     (lambda (controller request . args)
                       (add1 (apply (controller-body-proc controller) request args)))])
      (check-equal? (site-dispatch math (test-request "/divide/8/2")) 5))))
