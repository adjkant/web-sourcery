#lang racket

(provide member?
         when/f)

(require (for-syntax syntax/parse
                     racket/syntax))

(define (member? x xs)
  (cons? (member x xs)))

;; Top Level WebSourcery App Definition
(define-syntax when/f
  (syntax-parser
    [(_ condition if-true) #'(if condition if-true #false)]))