#lang racket

(provide define-web-sourcery-app
         define-route
         run-web-sourcery-app
         GET GET? POST POST? PUT PUT? DELETE DELETE? method->symbol
         (rename-out [user-response response]
                     [ws-status custom-status]
                     [ws-query-param query-param]
                     [ws-header header]
                     [ws-cookie cookie])
         (all-from-out "http/status-codes.rkt")
         json-obj json-kv
         json-serializer json-serializer-struct
         (all-from-out "request-testing/test-requests.rkt"))

(require web-server/servlet
         web-server/servlet-env
         "routing/routing.rkt"
         "http/methods.rkt"
         "http/status-codes.rkt"
         "data/defs.rkt"
         "response/response.rkt"
         "json/json.rkt"
         "json/serializers.rkt"
         "request-testing/test-requests.rkt"
         (for-syntax syntax/parse
                     racket/syntax))

(module+ test (require "utils/testing.rkt"))

;; Constants
(define DEFAULT-PORT 9000)
(define DEFAULT-CORS? #false)
(define DEFAULT-PUBLIC? #false)

;; Top Level WebSourcery App Definition
(define-syntax define-web-sourcery-app
  (syntax-parser
    [(_ app-name:id) #'(define app-name '())]))

(module+ test
  (check-compile-error (define-web-sourcery-app x y))
  (check-compile-error (define-web-sourcery-app))
  (check-compile-error (define-web-sourcery-app "string"))
  (check-compile-error (define-web-sourcery-app 1))
  (check-compile-error (define-web-sourcery-app 'symbol)))

;; Start a WebSourcery Server
;; TODO https://docs.racket-lang.org/syntax/wrapc.html?q=syntax%20contract
(define-syntax run-web-sourcery-app
  (syntax-parser
    [(_ app-name
        (~alt (~optional (~seq #:port port:integer)       #:defaults ([port #'DEFAULT-PORT]))
              (~optional (~seq #:cors? cors?:boolean)     #:defaults ([cors? #'DEFAULT-CORS?]))
              (~optional (~seq #:public? public?:boolean) #:defaults ([public? #'DEFAULT-PUBLIC?]))
              (~optional (~seq #:json-serializers serializers) #:defaults ([serializers '()])))
        ...)
     #'(serve/servlet #:port port
                      #:command-line? #t
                      #:servlet-regexp #rx""
                      #:listen-ip public?
                      #:stateless? #t
                      (λ (req)
                        (begin
                          #;(displayln (request-post-data/raw req))
                          (handle-any-request/external req app-name serializers))))]))

(module+ test
  (check-compile-error (begin
                         (define-web-sourcery-app app)
                         (run-web-sourcery-app app 2)))
  (check-compile-error (begin
                         (define-web-sourcery-app app)
                         (run-web-sourcery-app app #:portt 1)))
  
  (check-compile-error (begin
                         (define-web-sourcery-app app)
                         (run-web-sourcery-app app #:port "a"))))
