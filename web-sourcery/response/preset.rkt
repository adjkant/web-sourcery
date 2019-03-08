#lang racket

(provide (all-defined-out))

(require web-server/servlet)

(define DEFAULT-RESPONSE-404-ROUTE-NOT-FOUND-MESSAGE
  "WebSoucery: No Matching Route - 404 TODO")

(define RESPONSE-500-INVALID-RESPONSE-MESSAGE
  "Internal WebSoucery Error: route handler did not return a valid response")