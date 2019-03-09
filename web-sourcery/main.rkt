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
  (check-compile       (define-web-sourcery-app x))
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
  (check-compile-error (define-web-sourcery-app app)
                       (run-web-sourcery-app app 2))
  
  (check-compile-error (define-web-sourcery-app app)
                       (run-web-sourcery-app app #:portt 1))
  
  (check-compile-error (define-web-sourcery-app app)
                       (run-web-sourcery-app app #:port "a")))

;; Testing Route Syntax

(module+ test
  (check-compile
   (define-web-sourcery-app app)
   (define-route [app "/" [GET]] -> TEXT
     (response "Yay" 200-OK))
   "define-route failed")

  (check-compile
   (define-web-sourcery-app app)
   (define-route [app "/" [GET]] -> TEXT
     "anything can go in the body at compile time")
   "define-route failed")

  (check-compile
   (define-web-sourcery-app app)
   (define-route [app "/" [GET]] -> JSON
     "the type doesn't have to match either")
   "define-route failed")

  (check-compile-error
   (define-web-sourcery-app app)
   (define-route [app "/" [GET]] -> TEXTY
     "nonexistant types should fail")
   "bad define-route succeeded")

  (check-compile-error
   (define-web-sourcery-app app)
   (define-route [app "/" []] -> TEXT
     "you need at least one method")
   "bad define-route succeeded")

  (check-compile-error
   (define-web-sourcery-app app)
   (define-route [app "/" [GETTY]] -> TEXT
     "no made up methods")
   "bad define-route succeeded")

  (check-compile-error
   (define-web-sourcery-app app)
   (define-route [badapp "/" [GET]] -> TEXT
     "the app name must be defined")
   "bad define-route succeeded")

  (check-compile-error
   (define-web-sourcery-app app)
   (define-route [app "/" [GET]] --> TEXT
     "the arrow must be exact")
   "bad define-route succeeded")

  (check-compile-error
   (define-web-sourcery-app app)
   (define-route [app "/<int:x>" [GET]] -> TEXT
     "two routes cannot be analagous")
   (define-route [app "/<int:y>" [GET]] -> TEXT
     "two routes cannot be analagous")
   "bad define-route succeeded")

  (check-compile
   (define-web-sourcery-app app)
   (define-route [app "/<int:x>" [GET]] -> TEXT
     "two routes cannot be analagous")
   (define-route [app "/<int:y>" [POST]] -> TEXT
     "but they can be for different HTTP verbs")
   "define-route failed")

  (check-compile-error
   (define-web-sourcery-app app)
   (define-route [app "/<int:x>/<strng:x>" [GET]] -> TEXT
     "duplicate route params cannot exists")
   "bad define-route succeeded")

  ;; route testing template
  #;(check-compile-error
     (define-web-sourcery-app app)
     (define-route [app "/" [GET]] -> TEXT
       "")
     "bad define-route succeeded")

  )
