#lang racket

(provide check-request
         with-app-and-serializers)

(require json-sourcery
         "simulated-requests.rkt"
         "../http/methods.rkt"
         "../data/defs.rkt"
         "../utils/basics.rkt"
         rackunit)

(require (for-syntax syntax/parse
                     racket/syntax))

;; Check a request's data value and status code match
(define-syntax check-request
  (syntax-parser
    ;; TODO use the type to auto-convert data
    [(_ [app serializers method path
             (~alt (~optional (~seq #:query-params query-params)    #:defaults ([query-params #''()]))
                   (~optional (~seq #:headers in-headers)           #:defaults ([in-headers #''()]))
                   (~optional (~seq #:cookies in-cookies)           #:defaults ([in-cookies #''()]))
                   (~optional (~seq #:json json)                    #:defaults ([json #''none])))
             ...]
        (~literal ->)
        [type data status
              (~alt (~optional (~seq #:with-headers out-headers)  #:defaults ([out-headers #''()]))
                    (~optional (~seq #:with-cookies out-cookies)  #:defaults ([out-cookies #''()])))
              ...])
     (define is-json-check? (equal? 'JSON (syntax-e #'type)))
     #`(begin
         (define request-response (simulate-request app serializers method path
                                                    #:query-params query-params
                                                    #:headers in-headers
                                                    #:cookies in-cookies
                                                    #:json json))
         (define check-data-string (if (and #,is-json-check?
                                            (json-serializable? data serializers))
                                       (serialize-json data serializers)
                                       (when/f (not #,is-json-check?)
                                               data)))

         (when (false? check-data-string)
           (error 'check-request
                  "given data cannot be converted to JSON with the given serializers:  ~a"
                  data))
                                          
         (check-equal? (ws-response-status request-response)
                       status
                       (format "http status not equal in request: ~s ~a"
                               (ws-method-m method) path))
         
         (check-equal? (ws-response-data request-response)
                       check-data-string
                       (format "response data not equal in request: ~s ~a"
                               (ws-method-m method) path))

         (check-true (andmap (λ (h) (member? h (ws-response-headers request-response)))
                             out-headers)
                     (format (string-append "some headers not present in response for request: ~s ~a"
                                            " - expected headers: ~a - included headers: ~a")
                             (ws-method-m method) path
                             out-headers (ws-response-headers request-response)))

         (check-true (andmap (λ (c) (member? c (ws-response-cookies request-response)))
                             out-cookies)
                     (format (string-append "some cookies not present in response for request: ~s ~a"
                                            " - expected cookies: ~a - included cookies: ~a")
                             (ws-method-m method) path
                             out-cookies (ws-response-cookies request-response))))]))

(define-syntax with-app-and-serializers
  (syntax-parser
    [(_ app serializers
        ((~literal check-request) [method path options ...] (~literal ->) [type data status ...]) ...)
     #'(begin
         (check-request [app serializers method path options ...]
                        ->
                        [type data status ...]) ...)]))