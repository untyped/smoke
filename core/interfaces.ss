#lang scheme

; Interfaces -------------------------------------

(define application<%>
  (interface ()
    dispatch))

(define page<%>
  (interface ()
    get-http-code
    get-http-headers
    get-http-status
    get-content-type
    get-http-timestamp
    respond
    handle-request)) ; called when handling a callback... ought to be renamed really

; Provides ---------------------------------------

(provide/contract
 [application<%> interface?]
 [page<%>        interface?])
