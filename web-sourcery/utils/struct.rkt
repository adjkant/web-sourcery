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
     #'(when/f #true #;(and (boolean? id.supertype-id) id.supertype-id) ; TODO
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


;; Scratchwork

(define-syntax get-struct-type-id
  (syntax-parser
    [(_ maybe-struct)
     #'(when/f (struct? maybe-struct)
               (let-values ([(name i g n o r e !)
                             (struct-type-info (let-values ([(s _) (struct-info maybe-struct)]) s))])
                 name))]))

(define-syntax symbol->id
  (syntax-parser
    [(_ s)
     (define a (string-append "example"#;(symbol->string (syntax-e #'s)) "!"))
     (define b (substring a 0 (- (string-length a) 2)))
     (define c (substring a (string-length b) (sub1 (string-length a))))
     (define d (format-id #'s "~a~a" b c))
     #`(struct-named-accessors s)]))

;; Testing by example

#;(struct example [x ys] #:transparent)
;(struct namee [x ys] #:transparent)
#;(struct sub example [x y])

#;(get-struct-type-id 1)
#;(define struct-sym (get-struct-type-id (example 1 2)))
#;struct-sym

#;(sourcery-db "test.db")
#;(sourcery-struct x [(path STRING)])

#;(struct-named-accessors example)
#;(struct-named-accessors sub)
#;(struct-named-accessors x)