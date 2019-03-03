#lang racket

(provide user-response
         valid-response?
         JSON-MIME-TYPE
         ALL-RESPONSE-HEADERS)

(require web-server/servlet
         json
         "../data/defs.rkt"
         "../http/status-codes.rkt")

(define (user-response data status)
  (ws-response data status))

(module+ test (require "../utils/testing.rkt"))

(define JSON-MIME-TYPE #"application/json")
(define ALL-RESPONSE-HEADERS (list (header #"Server" #"WebSourcery (Racket)")))

;; Response ResponseType -> Boolean
(define (valid-response? resp type)
  (and (ws-response? resp)
       (valid-response-status? (ws-response-status resp))
       (or (and (symbol=? type 'TEXT) (string? (ws-response-data resp)))
           (and (symbol=? type 'JSON) (jsexpr? (ws-response-data resp))))))

(module+ test
  (check-true (valid-response? RESPONSE-1 'TEXT))
  (check-true (valid-response? RESPONSE-2 'JSON))
  (check-true (valid-response? RESPONSE-3 'JSON))
  (check-false (valid-response? RESPONSE-2 'TEXT))
  (check-false (valid-response? RESPONSE-3 'TEXT))
  (check-false (valid-response? BAD-RESPONSE-1 'TEXT))
  (check-false (valid-response? "a" 'TEXT)))
