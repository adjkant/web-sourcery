#lang racket

(require "../lib/web-sourcery.rkt")

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
  (session-path (session-create "/example")))


(define-route [app "/data"]
  (for/fold ((cur "")) ((s (sourcery-load session)))
    (string-append (session-path s) "<br>" cur)))


;; ---------------------------------------------------

;; Run Application from a custom port
(run-web-sourcery-app app
                      #:cors? #t
                      #:port 100)