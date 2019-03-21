#lang racket

(provide user-response
         with-headers
         with-cookies
         valid-response?
         JSON-MIME-TYPE
         ALL-RESPONSE-HEADERS)

(require web-server/servlet
         json
         "../../../json-sourcery/json-sourcery/main.rkt" #;json-sourcery
         "../data/defs.rkt"
         "../http/status-codes.rkt")

(module+ test (require "../utils/testing.rkt"))

(define JSON-MIME-TYPE #"application/json")
(define ALL-RESPONSE-HEADERS (list (header #"Server" #"WebSourcery (Racket)")))

;; Response ResponseType [List-of JSONSerializer] -> Boolean
;; determine if a response with the given type and serializers is valid
(define (valid-response? resp type serializers)
  (and (ws-response? resp)
       (valid-response-status? (ws-response-status resp))
       (or (and (symbol=? type 'TEXT) (string? (ws-response-data resp)))
           (and (symbol=? type 'JSON) (json-serializable? (ws-response-data resp) serializers)))))

(module+ test
  (check-true (valid-response? RESPONSE-1 'TEXT '()))
  (check-true (valid-response? RESPONSE-2 'JSON '()))
  (check-true (valid-response? RESPONSE-3 'JSON '()))
  (check-false (valid-response? RESPONSE-2 'TEXT '()))
  (check-false (valid-response? RESPONSE-3 'TEXT '()))
  (check-false (valid-response? BAD-RESPONSE-1 'TEXT '()))
  (check-false (valid-response? "a" 'TEXT '())))

;; Any StatusCode -> Response
;; Translate a user response into a WSResponse
(define (user-response data status)
  (ws-response data status '() '()))

;; Response [List-of Headers] -> Response
;; append the given headers to the response's headers if valid, otherwise erroring
(define (with-headers r headers)
  (define invalid-headers (filter (λ (h)
                                    (or (not (ws-header? h))
                                        (not (string? (ws-header-field h)))
                                        (not (string? (ws-header-value h)))))
                                  headers))
  (cond
    [(not (empty? invalid-headers)) (error 'with-headers "Invalid headers given: ~s" invalid-headers)]
    [(not (ws-response? r)) (error 'with-headers "Invalid response given for first argument")]
    [else (ws-response (ws-response-data r)
                       (ws-response-status r)
                       (append (ws-response-headers r) headers)
                       (ws-response-cookies r))]))

;; Response [List-of Cookies] -> Response
;; append the given headers to the response's cookies if valid, otherwise erroring
(define (with-cookies r cookies)
  (define invalid-cookies (filter (λ (c)
                                    (or (not (ws-cookie? c))
                                        (not (string? (ws-cookie-name c)))
                                        (not (string? (ws-cookie-value c)))))
                                  cookies))
  (cond
    [(not (empty? invalid-cookies)) (error 'with-cookies "Invalid cookies given: ~s" invalid-cookies)]
    [(not (ws-response? r)) (error 'with-cookies "Invalid response given for first argument")]
    [else (ws-response (ws-response-data r)
                       (ws-response-status r)
                       (ws-response-headers r)
                       (append (ws-response-cookies r) cookies))]))


