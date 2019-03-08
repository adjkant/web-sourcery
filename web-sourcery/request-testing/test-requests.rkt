#lang racket

(provide check-request
         with-app-and-serializers)

(require "simulated-requests.rkt"
         "../http/methods.rkt"
         "../data/defs.rkt"
         "../json/json.rkt"
         "../utils/basics.rkt"
         rackunit)

(require (for-syntax syntax/parse
                     racket/syntax))

;; Check a request's data value and status code match
(define-syntax check-request
  (syntax-parser
    ;; TODO use the type to auto-convert data
    [(_ [app serializers method path] (~literal ->) [type data status]
        (~alt (~optional (~seq #:query-params query-params)       #:defaults ([query-params #''()]))
              (~optional (~seq #:headers headers)                 #:defaults ([headers #''()]))
              (~optional (~seq #:cookies cookies)                 #:defaults ([cookies #''()])))
        ...)
     #`(begin
         (define request-response (simulate-request app serializers method path
                                                    #:query-params query-params
                                                    #:headers headers
                                                    #:cookies cookies))
         (define check-data-string (if (and #,(equal? 'JSON (syntax-e #'type))
                                            (json-serializable? data serializers))
                                       (serialize-json data serializers)
                                       (when/f (not #,(equal? 'JSON (syntax-e #'type)))
                                               data)))

         (when (false? check-data-string)
           (error 'check-request
                  "given data cannot be converted to JSON with the given serializers:  ~a"
                  data))
                                          
         (check-equal? (ws-response-status request-response)
                       status
                       (format "HTTP status not equal in request: ~s ~a"
                               (ws-method-m method) path))
         (check-equal? (ws-response-data request-response)
                       check-data-string
                       (format "response data not equal in request: ~s ~a"
                               (ws-method-m method) path)))]))

(define-syntax with-app-and-serializers
  (syntax-parser
    [(_ app serializers
        ((~literal check-request) [method path] (~literal ->) [type data status]) ...)
     #'(begin
         (check-request [app serializers method path] -> [type data status]) ...)]))