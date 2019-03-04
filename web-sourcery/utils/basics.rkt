#lang racket

(provide (all-defined-out))

(require (for-syntax syntax/parse
                     racket/syntax))

;; [Syntax-of Id] -> String
;; Convert an identifier to a string
(define (id->string id)
  (symbol->string (syntax-e id)))

;; Any [List-of Any] -> Boolean
;; Determine if the given item exists in the given list
(define (member? x xs)
  (cons? (member x xs)))

;; [List-of Any] -> Boolean
;; determine if two items exist in the list as according to equal?
(define (duplicates? xs)
  (cond [(empty? xs) #f]
        [(cons? xs) (or (member? (first xs) (rest xs))
                        (duplicates? (rest xs)))]))


;; [List-of X] [List-of Y] -> [List-of (list X Y)]
;; zip two equal sized lists together, erroring if the lists are not equal
(define (zip l1 l2)
  (map list l1 l2))


;; [List-of String] -> [List-of String]
;; if the last string in the list is an empty string
(define (trim-trailing-empty-string l)
  (cond [(empty? l) '()]
        [(empty? (rest l)) (if (string=? "" (first l)) '() l)]
        [else (cons (first l) (trim-trailing-empty-string (rest l)))]))

(define-syntax when/f
  (syntax-parser
    [(_ condition if-true) #'(if condition if-true #false)]))