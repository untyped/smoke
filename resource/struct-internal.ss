#lang scheme/base

(require net/url
         scheme/contract
         scheme/match
         scheme/pretty
         (only-in srfi/1 delete)
         (planet untyped/unlib:3/list)
         "../base.ss")

; Structure types --------------------------------

; (struct (listof web-resource)
;         (listof web-resource)
;         (U (listof web-resource) #f)
;         string)
;
; where prev is a list of immediate prerequisite resources;
;       next is a list of immediately dependent resources;
;       plan is an ordered list of all the resources to include to get this one to work.
;
; Plans are stored sorted from dependent to prerequisite (making plan construction easier).
;
; Whenever any prerequisites of a resource are changed, the resource's plan is invalidated and has to be recalculated.
; This is done lazily by web-resource-plan/recalculate. Invalid plans are set to #f until they are needed.
(define-struct web-resource (prev next plan mime-type) #:transparent #:mutable)

; (struct ... string)
(define-struct (compound-resource web-resource)
  (name)
  #:transparent
  #:property prop:custom-write
  (lambda (res out write?)
    (fprintf out "#(compound-resource ~s)" (compound-resource-name res))))

; (struct ...)
(define-struct (js-resource web-resource) () #:transparent #:mutable)

; (struct ... url)
(define-struct (js-file js-resource)
  (url)
  #:transparent
  #:mutable
  #:property prop:custom-write
  (lambda (res out write?)
    (fprintf out "#(js-resource ~s)" (url->string (js-resource-url res)))))

; (struct ... string (U js (any ... -> js))
(define-struct (js-script js-resource)
  (name content-generator)
  #:transparent
  #:mutable
  #:property prop:custom-write
  (lambda (res out write?)
    (fprintf out "#(js-resource ~s)" (url->string (js-resource-url res)))))

; (struct ... (listof string) boolean)
(define-struct (css-resource web-resource)
  (media ie-selector)
  #:transparent
  #:property prop:custom-write
  (lambda (res out write?)
    (fprintf out "#(css-resource ~s)" (url->string (css-resource-url res)))))

; (struct ... path)
(define-struct (css-file css-resource)
  (url)
  #:transparent
  #:property prop:custom-write
  (lambda (res out write?)
    (fprintf out "#(css-script ~s)" (url->string (css-resource-url res)))))

; (struct ... string (any ... -> string))
;
; The content-generator is a procedure that returns the content.
; Arguments to the content-generator are the same as arguments to
; render-web-resource and its variants.
(define-struct (css-script css-resource)
  (name content-generator)
  #:transparent
  #:property prop:custom-write
  (lambda (res out write?)
    (fprintf out "#(css-script ~s)" (url->string (css-resource-url res)))))

; Accessing plans --------------------------------

; web-resource web-resource -> boolean
;
; Returns #t if a depends on b, #f otherwise.
(define (web-resource-dependency? a b)
  (and (not (eq? a b))
       (memq b (web-resource-plan/recreate a))
       #t))

; web-resource web-resource -> boolean
(define (web-resource<? a b)
  (cond [(web-resource-dependency? a b) #t]
        [(web-resource-dependency? b a) #f]
        [(js-resource? a)
         (cond [(js-resource? b)
                (string<? (url->string (js-resource-url a))
                          (url->string (js-resource-url b)))]
               [(css-resource? b) #t]
               [(compound-resource? b) #t])]
        [(css-resource? a)
         (cond [(js-resource? b) #f]
               [(css-resource? b)
                (string<? (url->string (css-resource-url a))
                          (url->string (css-resource-url b)))]
               [(compound-resource? b) #t])]
        [(compound-resource? a)
         (cond [(js-resource? b) #f]
               [(css-resource? b) #f]
               [(compound-resource? b)
                (string<? (compound-resource-name a)
                          (compound-resource-name b))])]
        [else #f]))

; web-resource -> plan
; 
; Retrieves the plan from res, recreating it if necessary.
(define (web-resource-plan/recreate res)
  (or (web-resource-plan res)
      (let ([plan (create-plan res)])
        (set-web-resource-plan! res plan)
        plan)))

; web-resource -> plan
;
; Creates a plan 
(define (create-plan res)
  (cons res (merge-plans (map web-resource-plan/recreate (web-resource-prev res)))))

; (listof plan) -> plan
(define (merge-plans plans)
  (for/fold ([accum null])
            ([plan (in-list plans)])
            (merge-sorted-lists
             accum (sort plan web-resource<?) eq? web-resource<?)))

; Modifying plans --------------------------------

; web-resource web-resource -> void
;
; Adds a dependency of a on b.
(define (add-web-resource-dependency! a b)
  (cond [(web-resource-dependency? a b) (void)]
        [(web-resource-dependency? b a)
         (error (format "~a already depends on ~a" 
                        (web-resource->string b)
                        (web-resource->string a)))]
        [else (set-web-resource-prev! a (cons b (web-resource-prev a)))
              (set-web-resource-next! b (cons a (web-resource-next b)))
              ; Invalidate after adding to get the new dependents:
              (invalidate-plans! b)]))

; web-resource web-resource -> void
;
; Removes a dependency of a on b.
(define (remove-web-resource-dependency! a b)
  ; Invalidate before removing adding to get the old dependents:
  (invalidate-plans! b)
  (set-web-resource-prev! a (delete b (web-resource-prev a) eq?))
  (set-web-resource-next! b (delete a (web-resource-next b) eq?)))

; web-resource -> void
(define (invalidate-plans! res)
  (set-web-resource-plan! res #f)
  (for ([res (in-list (web-resource-next res))])
    (invalidate-plans! res)))

; Other procedures -------------------------------

; web-resource -> string
;
; Returns a short string representation of the resource (mostly for debugging purposes).
(define (web-resource->string resource)
  (cond [(js-resource? resource)       (url->string (js-resource-url resource))]
        [(css-resource? resource)      (url->string (css-resource-url resource))]
        [(compound-resource? resource) (compound-resource-name resource)]))

; Provide statements -----------------------------

(provide/contract
 [struct web-resource                     ([prev        (listof web-resource?)]
                                           [next        (listof web-resource?)]
                                           [plan        (or/c (listof web-resource?) false/c)])]
 [struct (compound-resource web-resource) ([prev        (listof web-resource?)]
                                           [next        (listof web-resource?)]
                                           [plan        (or/c (listof web-resource?) false/c)]
                                           [name        string?])]
 [struct (js-resource web-resource)       ([prev        (listof web-resource?)]
                                           [next        (listof web-resource?)]
                                           [plan        (or/c (listof web-resource?) false/c)]
                                           [url         url?])]
 [struct (css-resource web-resource)      ([prev        (listof web-resource?)]
                                           [next        (listof web-resource?)]
                                           [plan        (or/c (listof web-resource?) false/c)] 
                                           [url         url?] 
                                           [media       (listof string?)]
                                           [ie-selector (or/c string? false/c)])]
 [web-resource-dependency?                (-> web-resource? web-resource? boolean?)]
 [web-resource<?                          (-> web-resource? web-resource? boolean?)]
 [web-resource-plan/recreate              (-> web-resource? (listof web-resource?))]
 [add-web-resource-dependency!            (-> web-resource? web-resource? void?)]
 [remove-web-resource-dependency!         (-> web-resource? web-resource? void?)]
 [web-resource->string                    (-> web-resource? string?)])
