#lang racket

(provide
 GET GET?
 POST POST?
 PUT PUT?
 DELETE DELETE?
 method->symbol
 method->string)

(require "../data/defs.rkt")

(module+ test (require rackunit))

(define GET (ws-method 'GET))
(define POST (ws-method 'POST))
(define PUT (ws-method 'PUT))
(define DELETE (ws-method 'DELETE))

(define method->string (compose symbol->string ws-method-m))
(define method->symbol ws-method-m)

(define (GET? m)
  (symbol=? 'GET (ws-method-m m)))

(module+ test
  (check-true (GET? GET))
  (check-false (GET? POST))
  (check-false (GET? PUT))
  (check-false (GET? DELETE)))

(define (POST? m)
  (symbol=? 'POST (ws-method-m m)))

(module+ test
  (check-false (POST? GET))
  (check-true (POST? POST))
  (check-false (POST? PUT))
  (check-false (POST? DELETE)))

(define (PUT? m)
  (symbol=? 'PUT (ws-method-m m)))

(module+ test
  (check-false (PUT? GET))
  (check-false (PUT? POST))
  (check-true (PUT? PUT))
  (check-false (PUT? DELETE)))

(define (DELETE? m)
  (symbol=? 'DELETE (ws-method-m m)))

(module+ test
  (check-false (DELETE? GET))
  (check-false (DELETE? POST))
  (check-false (DELETE? PUT))
  (check-true (DELETE? DELETE)))

