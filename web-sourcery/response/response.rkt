#lang racket

(provide user-response)

(require "../data/defs.rkt")

(define (user-response data status)
  (ws-response data status))
  
