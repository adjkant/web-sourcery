#lang racket

(provide struct-named-accessors)

(require syntax/parse
         racket/syntax
         "basics.rkt")

(require (for-syntax racket/list
                     syntax/parse
                     syntax/parse/class/struct-id
                     racket/syntax))

;; [Id-of Struct] -> [Maybe [List-of Symbol]]
(define-syntax struct-field-names
  (syntax-parser
    [(_ id:struct-id)
     #`(when/f #,(and (boolean? (syntax-e #'id.supertype-id)) (syntax-e #'id.supertype-id))
               (let [(struct-name-length (add1 (string-length (symbol->string 'id))))]
                 (map string->symbol
                      (map (λ (a) (substring (symbol->string a) struct-name-length))
                           (list 'id.accessor-id ...)))))]))

;; [Id-of Struct] -> [Maybe [List-of (list Symbol [Struct -> Any])]]
(define-syntax struct-named-accessors
  (syntax-parser
    [(_ id:struct-id)
     #'(let [(field-names (struct-field-names id))]
         (when/f field-names
                 (zip field-names (list id.accessor-id ...))))]))