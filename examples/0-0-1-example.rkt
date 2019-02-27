#lang racket

(require "../web-sourcery/main.rkt")
(require sql-sourcery)

;; Specify a Database
(sourcery-db "0-0-1-server.db")

;; Create Sourcery Structures
(sourcery-struct session [(path STRING)])
(sourcery-struct data [(field STRING) (timestamp INTEGER)])

;; Define an application
(define-web-sourcery-app app)


;; Define Routes
;; ---------------------------------------------------

(define-route [app "" [GET]]
  (session-path (session-create "blank GET")))

(define-route [app "" [POST]]
  (session-path (session-create "blank POST")))

(define-route [app "/all-methods" [GET POST PUT DELETE]]
  (session-path
   (session-create
    (cond
      [(GET? method) "all methods GET"]
      [(POST? method) "all methods POST"]
      [(PUT? method) "all methods PUT"]
      [(DELETE? method) "all methods DELETE"]
      [else "no matching method"]))))

(define-route [app "/example" [GET]]
  (session-path (session-create "example")))

(define-route [app "/data" [GET]]
  (begin
    (session-path (session-create "/data"))
    (for/fold ((cur "")) ((s (sourcery-load session)))
      (string-append (session-path s) "<br>" cur))))

(define-route [app "/<int:param-num>" [GET]]
  (session-path (session-create (string-append "Matched an int: "
                                               (number->string param-num)))))

(define-route [app "/<string:param-string>" [GET]]
  (session-path (session-create (string-append "Matched a string: " param-string))))

(define-route [app "/query-param-value-x" [GET]]
  (session-path (session-create (format "query param x value: ~s" (query-param "x")))))

(define-route [app "/header/<string:header-field>" [GET]]
  (if (headers header-field)
      (headers header-field)
      "No Matching Header"))

(define-route [app "/cookie/<string:cookie-name>" [GET]]
  (if (cookies cookie-name)
      (cookies cookie-name)
      "No Matching Cookie"))

;; ---------------------------------------------------

;; Run Application from a custom port
(run-web-sourcery-app app
                      #:cors? #t
                      #:port 1000)