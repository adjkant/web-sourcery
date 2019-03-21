#lang racket

(provide simulate-request)

(require web-server/http
         json-sourcery
         "../routing/routing.rkt"
         "../http/methods.rkt"
         "../data/defs.rkt"
         "../data/conversion.rkt"
         "../utils/basics.rkt")

(require (for-syntax syntax/parse
                     racket/syntax
                     "../utils/basics.rkt"))

;; Simulate any request with any given method
(define-syntax simulate-request
  (syntax-parser
    [(_ app serializers method path
        (~alt (~optional (~seq #:query-params query-params)       #:defaults ([query-params #''()]))
              (~optional (~seq #:headers headers)                 #:defaults ([headers #''()]))
              (~optional (~seq #:cookies cookies)                 #:defaults ([cookies #''()]))
              (~optional (~seq #:json json)                       #:defaults ([json #''none])))
        ...)
     #'(let* [(request-path
               (strings->request-path (trim-trailing-empty-string (string-split path "/"))))
              (req-json (if (equal? json 'none) #f json))
              (json-src (if (equal? json 'none) (string->symbol "none") (string->symbol "json")))
              (internal-req
               (ws-request method request-path query-params headers cookies req-json json-src '()))]
         (convert-response-data-and-errors
          (handle-any-request/internal internal-req app serializers)
          serializers))]))

;; Response [List-of JSONSerializer] -> (ws-response String StatusCode)
;; stringify the given responses data only
(define (convert-response-data-and-errors resp serializers)
  (cond
    [(ws-response? resp)
     (ws-response (serialize-json (ws-response-data resp) serializers)
                  (ws-response-status resp)
                  (ws-response-headers resp)
                  (ws-response-cookies resp))]
    [(natural? resp)
     (ws-response (response-error-code->string resp)
                  (response-error-code->status resp)
                  '() '())]))
