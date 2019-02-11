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

(define-route [app "/example"]
  (session-path (session-create "/z")))

(define-route [app "/data"]
  (begin
    (session-path (session-create "/data"))
    (for/fold ((cur "")) ((s (sourcery-load session)))
      (string-append (session-path s) "<br>" cur))))

(define-route [app "/<int:todo>"]
  (session-path (session-create "Matched an int!")))

(define-route [app "/<string:todo>"]
  (session-path (session-create "Matched a string!")))


;; ---------------------------------------------------

;; Run Application from a custom port
(run-web-sourcery-app app
                      #:cors? #t
                      #:port 1000)