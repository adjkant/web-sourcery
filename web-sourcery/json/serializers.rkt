#lang racket

(provide
 json-serializer
 json-serializer-struct)

(require "../utils/struct.rkt"
         "../utils/basics.rkt"
         "../data/defs.rkt")

(require (for-syntax syntax/parse
                     racket/syntax
                     syntax/parse/class/struct-id
                     "../utils/struct.rkt"
                     "../utils/basics.rkt"
                     "../data/defs.rkt"))

(define-syntax json-serializer
  (syntax-parser
    [(_ predicate (field:id getter:id) ...)
     (define field-strings (cons list (map id->string (syntax->list #'(field ...)))))
     #`(ws-json-serializer predicate
                           (zip (map string->symbol #,field-strings)
                                (list getter ...)))]))

(define-syntax json-serializer-struct
  (syntax-parser
    [(_ s:struct-id)
     (define struct-predicate (format-id #'s "~a?" (syntax-e #'s)))
     #`(let [(named-accessors-list (struct-named-accessors s))]
         (if named-accessors-list
             (ws-json-serializer #,struct-predicate named-accessors-list)
             (error 'TODO-ERROR "cannot serialize a subtyped strcture automatically ~a" s)))]))
