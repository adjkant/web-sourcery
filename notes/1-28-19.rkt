#lang racket

(struct foo (bar) #:property prop:procedure 0)

(define f (foo (λ (x) x)))

(f 10)

;;; inherit from structs for cookies/headers


;; fromlets for HTML templating


;; package for include/require