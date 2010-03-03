#lang scheme

(require "../class/class-internal.ss")

; Interfaces -------------------------------------

(define component<%>
  (interface (object/cells<%>)
    get-component-id       ; -> symbol
    on-request             ; request -> void
    render                 ; seed -> content
    get-child-components   ; -> (listof component<%>)
    get-all-components     ; -> (listof component<%>)
    get-dirty-components   ; -> (listof component<%>)
    find-component         ; symbol -> (U component<%> #f)
    custom-print))         ; output-port boolean (U symbol #f) -> void

; The site is the core of the Smoke application.
; It defines default behaviours across all pages,
; and is the first point of dispatch for HTTP requests.
; There should be exactly one site per application,
; defined using the define-site macro.

(define site<%>
  (interface (component<%>)
    dispatch))

; The page is the unit of functionality in a web application.
; It contains methods for responding to requests, rendering HTML,
; checking access privileges and so on.

(define page<%>
  (interface* (component<%>)
    ([prop:procedure
      (lambda (this . args)
        (send this dispatch-initial . args))])
    get-http-code
    get-http-headers
    get-http-status
    get-content-type
    get-http-timestamp
    dispatch-initial
    dispatch-callback
    respond
    access-allowed?
    access-denied))

; Provides ---------------------------------------

(provide/contract
 [component<%>  interface?]
 [site<%>       interface?]
 [page<%>       interface?])
