#lang web-server

(require scheme/contract
         (prefix-in json: (planet dherman/json:1/json)))

; Procedures -------------------------------------

; string -> json-serializable
(define (scheme->json data)
  (let ([out (open-output-string)])
    (json:write data out)
    (get-output-string out)))

; string -> json-serializable
(define (json->scheme json)
  (let ([in (open-input-string json)])
    (json:read in)))

; Provide statements -----------------------------

(provide (rename-out [json:json? json-serializable?]))

(provide/contract
 [json->scheme (-> string? json:json?)]
 [scheme->json (-> json:json? string?)])
