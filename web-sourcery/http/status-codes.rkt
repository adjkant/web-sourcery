#lang racket

(provide (all-defined-out))

(require "../data/defs.rkt")

(define 200-OK                  (ws-status 200 "Ok"))
(define 201-CREATED             (ws-status 201 "Created"))
(define 202-ACCEPTED            (ws-status 202 "Accepted"))
(define 203-NON-AUTH            (ws-status 203 "TODO"))
(define 204-NO-CONTENT          (ws-status 204 "TODO"))
(define 206-PARTIAL-CONTENT     (ws-status 206 "TODO"))
(define 301-MOVED-PERM          (ws-status 301 "TODO"))
(define 307-TEMP-REDIRECT       (ws-status 307 "TODO"))
(define 400-BAD-REQUEST         (ws-status 400 "TODO"))
(define 401-UNAUTH              (ws-status 401 "TODO"))
(define 403-FORBIDDEN           (ws-status 403 "TODO"))
(define 404-NOT-FOUND           (ws-status 404 "TODO"))
(define 405-METHOD-NOT-ALLOWED  (ws-status 405 "TODO"))
(define 500-INTERNAL-ERROR      (ws-status 500 "TODO"))
(define 501-NOT-IMPLEMENTED     (ws-status 501 "TODO"))
(define 503-UNAVAILABLE         (ws-status 503 "TODO"))