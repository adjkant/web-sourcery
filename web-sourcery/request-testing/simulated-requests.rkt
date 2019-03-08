#lang racket

(provide simulate-request)

(require web-server/http
         "../routing/routing.rkt"
         "../http/methods.rkt"
         "../data/defs.rkt"
         "../data/conversion.rkt"
         "../json/json.rkt"
         "../utils/basics.rkt")

(require (for-syntax syntax/parse
                     racket/syntax))

;; Simulate any request with any given method
(define-syntax simulate-request
  (syntax-parser
    [(_ app serializers method path
        (~alt (~optional (~seq #:query-params query-params)       #:defaults ([query-params #''()]))
              (~optional (~seq #:headers headers)                 #:defaults ([headers #''()]))
              (~optional (~seq #:cookies cookies)                 #:defaults ([cookies #''()])))
        ...)
     #'(let* [(request-path
               (strings->request-path (trim-trailing-empty-string (string-split path "/"))))
              (internal-req
               (ws-request method request-path query-params headers cookies))]
         (stringify-response-data-and-errors
          (handle-any-request/internal internal-req app serializers)
          serializers))]))

;; Response [List-of JSONSerializer] -> (ws-response String StatusCode)
;; stringify the given responses data only
(define (stringify-response-data-and-errors resp serializers)
  (cond
    [(ws-response? resp)
     (ws-response (serialize-json (ws-response-data resp) serializers) (ws-response-status resp))]
    [(natural? resp)
     (let [(external-response (response-error-code->response resp))]
       (ws-response (bytes->string/utf-8 (first (response-output external-response)))
                    (ws-status (response-code external-response)
                               (bytes->string/utf-8 (response-message external-response)))))]))
