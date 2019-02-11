#lang racket

(provide define-web-sourcery-app
         define-route
         run-web-sourcery-app)

(require web-server/servlet
         web-server/servlet-env
         "routing.rkt"
         (for-syntax syntax/parse
                     racket/syntax))

;; A WSApp is a [List-of Route]

;; Constants
(define DEFAULT-PORT 9000)
(define DEFAULT-CORS? #false)
(define DEFAULT-PUBLIC? #false)

;; Top Level WebSourcery App Definition
(define-syntax define-web-sourcery-app
  (syntax-parser
    [(_ app-name:id) #'(define app-name '())]))

;; Start a WebSourcery Server
;; TODO https://docs.racket-lang.org/syntax/wrapc.html?q=syntax%20contract
(define-syntax run-web-sourcery-app
  (syntax-parser
    [(_ app-name
        (~alt (~optional (~seq #:port port:integer)       #:defaults ([port #'DEFAULT-PORT]))
              (~optional (~seq #:cors? cors?:boolean)     #:defaults ([cors? #'DEFAULT-CORS?]))
              (~optional (~seq #:public? public?:boolean) #:defaults ([public? #'DEFAULT-PUBLIC?])))
        ...)
     #'(serve/servlet #:port port
                      #:command-line? #t
                      #:servlet-regexp #rx""
                      #:listen-ip public?
                      #:stateless? #t
                      (λ (req)
                        (response/full 200
                                       #"Ok"
                                       (current-seconds)
                                       TEXT/HTML-MIME-TYPE
                                       '()
                                       (list (match-request-to-route req app-name)))))]))
