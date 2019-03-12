#lang racket

(provide (all-from-out rackunit)
         (all-defined-out))

(require rackunit
         "../http/status-codes.rkt"
         "../data/defs.rkt"
         "../http/methods.rkt")

(require (for-syntax syntax/parse
                     rackunit))

(define-syntax check-error
  (syntax-parser
    [(_ exn-expr failure-message:string) #'(check-exn exn:fail? (λ () exn-expr) failure-message)]
    [(_ exn-expr) #'(check-exn exn:fail? (λ () exn-expr))]))

(define-syntax check-not-error
  (syntax-parser
    [(_ expr failure-message:string) #'(check-not-exn (λ () expr) failure-message)]
    [(_ expr) #'(check-not-exn (λ () expr))]))

(define-syntax check-compile
  (syntax-parser
    [(_ cmp-exn-expr ... failure-message:string)
     #`(check-not-error (eval-syntax #`(begin (require "main.rkt") #,#'cmp-exn-expr ...))
                        failure-message)]
    [(_ cmp-exn-expr ...)
     #`(check-not-error (eval-syntax #`(begin (require "main.rkt") #,#'cmp-exn-expr ...)))]))

(define-syntax check-compile-error
  (syntax-parser
    [(_ cmp-exn-expr ... failure-message:string)
     #`(check-error (eval-syntax #`(begin (require "main.rkt") #,#'cmp-exn-expr ...))
                    failure-message)]
    [(_ cmp-exn-expr ...)
     #`(check-error (eval-syntax #`(begin (require "main.rkt") #,#'cmp-exn-expr ...)))]))

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

(define QUERY-PARAM-1 (ws-query-param "x" "y"))
(define QUERY-PARAM-2 (ws-query-param "num" "1"))
(define QUERY-PARAM-3 (ws-query-param "sortOrder" "max"))

(define QUERY-PARAMS-1 (list QUERY-PARAM-1 QUERY-PARAM-2 QUERY-PARAM-3))

(define COOKIE-1 (ws-cookie "a" "b"))
(define COOKIE-2 (ws-cookie "num" "1"))
(define COOKIE-3 (ws-cookie "sessionToken" "h4ck3r"))

(define COOKIES-1 (list COOKIE-1 COOKIE-2 COOKIE-3))

(define HEADER-1 (ws-header "Content-Type" "application/json"))
(define HEADER-2 (ws-header "timestamp" "1550796402"))
(define HEADER-3 (ws-header "secret" "shhhhh"))

(define HEADERS-1 (list HEADER-1 HEADER-2 HEADER-3))

(define REQ-1 (ws-request GET REQ-PATH-1 #f '() '()))
(define REQ-2 (ws-request GET REQ-PATH-2 #f HEADERS-1 COOKIES-1))
(define REQ-3 (ws-request GET REQ-PATH-3 #f HEADERS-1 COOKIES-1))
(define REQ-4 (ws-request GET REQ-PATH-4 #f HEADERS-1 COOKIES-1))

(define ROUTE-PARAM-STR (ws-route-param "matched-string" 'string))
(define ROUTE-PARAM-INT (ws-route-param "matched-int" 'int))
(define ROUTE-PARAM-I1  (ws-route-param "i1" 'int))
(define ROUTE-PARAM-I2  (ws-route-param "i2" 'int))
(define ROUTE-PARAM-I3  (ws-route-param "i3" 'int))

(define PATH-TEMP-0 '())
(define PATH-TEMP-1 (list "hello" "world"))
(define PATH-TEMP-2 (list "hello" "world" "a" "bit" "longer"))
(define PATH-TEMP-3 (list ROUTE-PARAM-STR))
(define PATH-TEMP-4 (list ROUTE-PARAM-INT))
(define PATH-TEMP-5 (list ROUTE-PARAM-STR ROUTE-PARAM-INT))
(define PATH-TEMP-6 (list ROUTE-PARAM-I1 ROUTE-PARAM-I2 ROUTE-PARAM-I3))
(define PATH-TEMP-7 (list "hello" ROUTE-PARAM-STR))
(define PATH-TEMP-8 (list ROUTE-PARAM-I1))

(define ROUTE-0 (ws-route PATH-TEMP-0
                          (list GET)
                          'TEXT
                          (λ ()
                            "Hello World Blank!")))
(define ROUTE-1 (ws-route PATH-TEMP-1
                          (list GET)
                          'TEXT
                          (λ ()
                            "Hello World 1!")))
(define ROUTE-2 (ws-route PATH-TEMP-2
                          (list GET)
                          'TEXT
                          (λ ()
                            "Hello World Long!")))
(define ROUTE-3 (ws-route PATH-TEMP-3
                          (list GET)
                          'TEXT
                          (λ (matched-string)
                            matched-string)))
(define ROUTE-4 (ws-route PATH-TEMP-4
                          (list GET)
                          'TEXT
                          (λ (matched-int)
                            (number->string matched-int))))
(define ROUTE-5 (ws-route PATH-TEMP-5
                          (list GET)
                          'TEXT
                          (λ (matched-string matched-int)
                            (string-append matched-string " " (number->string matched-int)))))
(define ROUTE-6 (ws-route PATH-TEMP-7
                          (list GET)
                          'TEXT
                          (λ (matched-string matched-int)
                            (string-append "Hello " matched-string "!"))))
(define ROUTE-7 (ws-route PATH-TEMP-8
                          (list GET)
                          'TEXT
                          (λ (matched-string matched-int)
                            "got an int and calling it i")))
(define ROUTE-8 (ws-route PATH-TEMP-8
                          (list POST)
                          'TEXT
                          (λ (matched-string matched-int)
                            "got an int with a post and calling it i")))

(define MATCHED-ROUTE-0 (ws-matched-route ROUTE-0 PATH-TEMP-0 '()))
(define MATCHED-ROUTE-1 (ws-matched-route ROUTE-1 PATH-TEMP-1 '(exact exact)))
(define MATCHED-ROUTE-2 (ws-matched-route ROUTE-1 PATH-TEMP-1 '(#false #false)))
(define MATCHED-ROUTE-3 (ws-matched-route ROUTE-3 PATH-TEMP-3 '(param)))
(define MATCHED-ROUTE-4 (ws-matched-route ROUTE-4 PATH-TEMP-4 '(param)))
(define MATCHED-ROUTE-5 (ws-matched-route ROUTE-4 PATH-TEMP-4 '(#false)))
(define MATCHED-ROUTE-6 (ws-matched-route ROUTE-1 PATH-TEMP-7 '(exact param)))

(define MATCHED-ROUTES-1 (list MATCHED-ROUTE-1 MATCHED-ROUTE-6))

(define RESPONSE-1 (ws-response "Hello World" 200-OK))
(define RESPONSE-2 (ws-response 1 200-OK))
(define RESPONSE-3 (ws-response (list 1 2 3) 200-OK))

(define BAD-RESPONSE-1 (ws-response "Hello World" 1))




