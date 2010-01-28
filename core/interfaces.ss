#lang scheme

(require "../class/class.ss")

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

(define page<%>
  (interface (component<%>)
    get-http-code
    get-http-headers
    get-http-status
    get-content-type
    get-http-timestamp
    respond
    handle-request))

(define application<%>
  (interface (component<%>)
    dispatch
    find-page+component)) ; symbol -> (U page<%> #f) (U component<%> #f)

; Provides ---------------------------------------

(provide/contract
 [component<%>   interface?]
 [application<%> interface?]
 [page<%>        interface?])
