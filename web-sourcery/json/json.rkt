#lang racket

(provide json-serializable?
         serialize-json
         json-obj
         json-kv)

(require json
         "../data/defs.rkt")

(require (for-syntax syntax/parse
                     racket/syntax))

;; ResponseData [List-of JSONSerializer] -> Boolean
(define (json-serializable? rd serializers)
  (define matched-serializer (find-matching-serializer rd serializers))
  (if matched-serializer
      (andmap (λ (f) (json-serializable? ((second f) rd) serializers))
              (ws-json-serializer-fields matched-serializer))
      (jsexpr? rd)))

;; ResponseData [List-of JSONSerializer] -> JSExpr
(define (serialize-json rd serializers)
  (define matched-serializer (find-matching-serializer rd serializers))
  (if matched-serializer
      (apply hash
             (foldr append '()
                    (map
                     (λ (f)
                       (serialize-json (json-kv (first f) ((second f) rd)) serializers))
                     (ws-json-serializer-fields matched-serializer))))
      rd))

;; ResponseData [List-of JSONSerializer] -> [Maybe JSONSerializer]
(define (find-matching-serializer rd serializers)
  (cond [(empty? serializers) #false]
        [(cons? serializers)
         (if ((ws-json-serializer-predicate (first serializers)) rd)
             (first serializers)
             (find-matching-serializer rd (rest serializers)))]))


;; shorthand for creating a hash in the context of JSON
(define-syntax json-obj
  (syntax-parser
    [(_ json-kv ...)
     #'(apply hash (foldr append '() (list json-kv ...)))]))


;; way to more clearly pair JSON values
(define-syntax json-kv
  (syntax-parser
    [(_ k v) #'(list k v)]))
