#lang racket

(provide
 request->ws-request
 ws-response->response
 strings->request-path)

(require web-server/servlet
         "defs.rkt"
         "../response/preset.rkt"
         "../utils/basics.rkt")

(module+ test (require "../utils/testing.rkt"))

;; TODO testing - need request generation

;; web-server/http/request -> Request
;; Convert to an internal request representation
(define (request->ws-request req)
  (ws-request
   (request->ws-method req)
   (strings->request-path (trim-trailing-empty-string (map path/param-path
                                                           (url-path (request-uri req)))))
   (request->ws-query-params req)
   (request->ws-headers req)
   (request->ws-cookies req)))

;; Response -> web-server/http/response
;; Create a response from the given string
(define (ws-response->response r)
  (if (and (ws-response? r) (string? (ws-response-data r)))
      (response/full (ws-status-code (ws-response-status r))
                     (string->bytes/utf-8 (ws-status-description (ws-response-status r)))
                     (current-seconds)
                     TEXT/HTML-MIME-TYPE
                     '()
                     (list (string->bytes/utf-8 (ws-response-data r))))
      RESPONSE-500-INVALID-RESPONSE))

;; String -> RequestPath
;; convert a string into a list of reuqest path parts
(define (strings->request-path path-part-strings)
  (map string->request-path-part path-part-strings))

(module+ test
  (check-equal? (strings->request-path (list "hello" "world")) REQ-PATH-1)
  (check-equal? (strings->request-path (string-split "/hello/world/a/bit/longer" "/")) REQ-PATH-2)
  (check-equal? (strings->request-path (list "1")) REQ-PATH-4)
  (check-equal? (strings->request-path (list "1" "2" "3")) REQ-PATH-5)
  (check-equal? (strings->request-path (list "-1")) REQ-PATH-6))
  

;; String -> RequestPathPart
;; convert a single string into a request path part
(define (string->request-path-part part)
  (if (and (string->number part) (integer? (string->number part)))
      (ws-req-path-part part '(int string))
      (ws-req-path-part part '(string))))

(module+ test
  (check-equal? (string->request-path-part "test") (ws-req-path-part "test" '(string)))
  (check-equal? (string->request-path-part "1") (ws-req-path-part "1" '(int string)))
  (check-equal? (string->request-path-part "-1") (ws-req-path-part "-1" '(int string))))


;; web-server/http/request -> Method
;; Get a Method from a request
(define (request->ws-method req)
  (define method-symbol (string->symbol (string-upcase (bytes->string/utf-8 (request-method req)))))
  (ws-method method-symbol))


;; web-server/http/request -> [List-of QueryParam]
;; Get a list of query params from a request
(define (request->ws-query-params req)
  (map query-param->ws-query-param (url-query (request-uri req))))

;; [Pair Symbol [Maybe String]] -> QueryParam
;; Get a structured QueryParam from a query param 
(define (query-param->ws-query-param qp)
  (ws-query-param (symbol->string (car qp)) (cdr qp)))


;; web-server/http/request -> [List-of Cookie]
;; Get a list of cookies from a request
(define (request->ws-cookies req)
  (map client-cookie->ws-cookie (request-cookies req)))


;; client-cookie -> Cookie
;; convert a client-cookie to a ws-cookie
(define (client-cookie->ws-cookie c)
  (ws-cookie (client-cookie-name c)
             (client-cookie-value c)))


;; web-server/http/request -> [List-of Header]
;; Get a list of headers from a request
(define (request->ws-headers req)
  (map header->ws-header (request-headers/raw req)))


;; web-server/http/request-structs/header -> Header
;; convert a header struct to a ws-header
(define (header->ws-header h)
  (ws-header (bytes->string/utf-8 (header-field h))
             (bytes->string/utf-8 (header-value h))))