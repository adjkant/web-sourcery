#lang racket

(provide route
         create-web-sourcery-app
         run-web-sourcery-app
         (all-from-out "../include/sql-sourcery/lib/sql-sourcery.rkt"))

(require web-server/servlet
         web-server/servlet-env
         (for-syntax syntax/parse)
         "../include/sql-sourcery/lib/sql-sourcery.rkt")

(define-syntax create-web-sourcery-app
  (syntax-parser
    [(_ app-name) #'(define app-name '())]))

(define-syntax route
  (syntax-parser
    [(_ app-name:id path:string (define (route-func) route-body))
        #'(begin
            (define (route-func) route-body)
            (set! app-name (cons (list path route-func) app-name)))]))

(define (handle-route req app)
  (let* [(path (first (map path/param-path (url-path (request-uri req)))))
         (matched-route (filter (λ (r) (string=? (substring (first r) 1) path)) app))]
    (if (> (length matched-route) 0)
        (string->bytes/utf-8 ((second (first matched-route))))
        (string->bytes/utf-8 "no matching route"))))

(define-syntax run-web-sourcery-app
  (syntax-parser
    [(_ app-name) #'(begin
                      (define (app-main req)
                        (response/full 200
                                       #"Ok"
                                       (current-seconds)
                                       TEXT/HTML-MIME-TYPE
                                       '()
                                       (list (handle-route req app-name))))
                      (serve/servlet app-main #:port 80
                                     #:command-line? #t
                                     #:servlet-regexp #rx""
                                     #:listen-ip #f
                                     #:stateless? #t))]))
 
#|(define (my-app req)
  (response/full 200
                 #"Ok"
                 (current-seconds)
                 TEXT/HTML-MIME-TYPE
                 '()
                 (list (string->bytes/utf-8
                        (first (map path/param-path
                                    (url-path (request-uri req))))))))
 
(serve/servlet my-app
               #:port 80
               #:command-line? #t
               #:servlet-regexp #rx""
               #:listen-ip #f
               #:stateless? #t)|#
