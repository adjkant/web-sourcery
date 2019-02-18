#lang racket

(provide

 ;; Basic Testing via Racket
 (all-from-out rackunit)

 ;; Checking compile-time errors
 compile
 syntax

 ;; All In File
 (all-defined-out)
 )

(require rackunit
         "../data-defs.rkt"
         (for-syntax syntax/parse
                     rackunit))

(define-syntax check-error
  (syntax-parser
    [(_ exn-expr failure-message:string) #'(check-exn exn:fail? (λ () exn-expr) failure-message)]
    [(_ exn-expr) #'(check-exn exn:fail? (λ () exn-expr))]))

(define REQ-PART-1 (ws-req-path-part "hello" '(string)))
(define REQ-PART-2 (ws-req-path-part "world" '(string)))
(define REQ-PART-3 (ws-req-path-part "a" '(string)))
(define REQ-PART-4 (ws-req-path-part "bit" '(string)))
(define REQ-PART-5 (ws-req-path-part "longer" '(string)))
(define REQ-PART-6 (ws-req-path-part "s" '(string)))
(define REQ-PART-7 (ws-req-path-part "1" '(int string)))
(define REQ-PART-8 (ws-req-path-part "2" '(int string)))
(define REQ-PART-9 (ws-req-path-part "3" '(int string)))
(define REQ-PART-10 (ws-req-path-part "-1" '(int string)))
(define REQ-PART-11 (ws-req-path-part "b" '(string)))
(define REQ-PART-12 (ws-req-path-part "c" '(string)))

(define REQ-PATH-1 (list REQ-PART-1 REQ-PART-2))
(define REQ-PATH-2 (list REQ-PART-1 REQ-PART-2 REQ-PART-3 REQ-PART-4 REQ-PART-5))
(define REQ-PATH-3 (list REQ-PART-6))
(define REQ-PATH-4 (list REQ-PART-7))
(define REQ-PATH-5 (list REQ-PART-7 REQ-PART-8 REQ-PART-9))
(define REQ-PATH-6 (list REQ-PART-10))

(define ROUTE-PARAM-STR (ws-route-param "matched-string" 'string))
(define ROUTE-PARAM-INT (ws-route-param "matched-int" 'int))
(define ROUTE-PARAM-I1  (ws-route-param "i1" 'int))
(define ROUTE-PARAM-I2  (ws-route-param "i2" 'int))
(define ROUTE-PARAM-I3  (ws-route-param "i3" 'int))

(define PATH-TEMP-1 (list "hello" "world"))
(define PATH-TEMP-2 (list "hello" "world" "a" "bit" "longer"))
(define PATH-TEMP-3 (list ROUTE-PARAM-STR))
(define PATH-TEMP-4 (list ROUTE-PARAM-INT))
(define PATH-TEMP-5 (list ROUTE-PARAM-STR ROUTE-PARAM-INT))
(define PATH-TEMP-6 (list ROUTE-PARAM-I1 ROUTE-PARAM-I2 ROUTE-PARAM-I3))
(define PATH-TEMP-7 (list "hello" ROUTE-PARAM-STR))

(define ROUTE-1 (ws-route PATH-TEMP-1
                          (λ ()
                            "Hello World 1!")))
(define ROUTE-2 (ws-route PATH-TEMP-2
                          (λ ()
                            "Hello World Long!")))
(define ROUTE-3 (ws-route PATH-TEMP-3
                          (λ (matched-string)
                            matched-string)))
(define ROUTE-4 (ws-route PATH-TEMP-4
                          (λ (matched-int)
                            (number->string matched-int))))
(define ROUTE-5 (ws-route PATH-TEMP-5
                          (λ (matched-string matched-int)
                            (string-append matched-string " " (number->string matched-int)))))
(define ROUTE-6 (ws-route PATH-TEMP-7
                          (λ (matched-string matched-int)
                            (string-append "Hello " matched-string "!"))))

(define MATCHED-ROUTE-1 (ws-matched-route ROUTE-1 PATH-TEMP-1 '(exact exact)))
(define MATCHED-ROUTE-2 (ws-matched-route ROUTE-1 PATH-TEMP-1 '(#false #false)))
(define MATCHED-ROUTE-3 (ws-matched-route ROUTE-3 PATH-TEMP-3 '(param)))
(define MATCHED-ROUTE-4 (ws-matched-route ROUTE-4 PATH-TEMP-4 '(param)))
(define MATCHED-ROUTE-5 (ws-matched-route ROUTE-4 PATH-TEMP-4 '(#false)))
(define MATCHED-ROUTE-6 (ws-matched-route ROUTE-1 PATH-TEMP-7 '(exact param)))

(define MATCHED-ROUTES-1 (list MATCHED-ROUTE-1 MATCHED-ROUTE-6))





