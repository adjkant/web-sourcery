#lang racket

(provide (all-defined-out))

(require web-server/servlet)

(define DEFAULT-RESPONSE-404-ROUTE-NOT-FOUND
  (response/full 404
                 #"Not Found"
                 (current-seconds)
                 TEXT/HTML-MIME-TYPE
                 '()
                 (list (string->bytes/utf-8 "WebSoucery: No Matching Route - 404 TODO"))))

(define RESPONSE-500-INVALID-RESPONSE
  (response/full 500
                 #"Internal Server Error"
                 (current-seconds)
                 TEXT/HTML-MIME-TYPE
                 '()
                 (list (string->bytes/utf-8
                        "Internal WebSoucery Error: route handler did not return a valid response"))))