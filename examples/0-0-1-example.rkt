#lang racket

(require "../lib/web-sourcery.rkt")
(sourcery-db "0-0-1-server.db")

(sourcery-struct session [(path STRING)])

(create-web-sourcery-app app)

(route app "/example"
(define (test-1)
  (session-path (session-create "/example"))))

(route app "/data"
(define (test-2)
  (foldl (λ (s cur) (string-append (session-path s) " " cur)) "" (sourcery-load session))))

(run-web-sourcery-app app)