#lang racket

(require "../web-sourcery/main.rkt")


(define-web-sourcery-app app)

(web-sourcery-static-file-path app "example/path/")


;; A basic "Hello World" route
(route app "/hello-world" (methods GET))
(define (hello-world query-params headers cookies)
  (web-sourcery-response "Hello World" 200))


;; Accept POST JSON Data and give back the first pair only
(route app "/accept-json" (methods POST))
(define (accept-json headers cookies data)
  (convert-to-json (web-sourcery-response (first data) 200)))


;; Route Parameters
(route app "/<int:x>" (methods GET POST UPDATE DELETE))
(define (any-int route-params headers cookies)
  (web-sourcery-response (route-params "x") 200))


;; Headers and Cookies Access
(route app "/headers-and-cookies" (methods GET))
(define (headers-cookies query-params headers cookies)
  (web-sourcery-response
   (string-append (headers "Content-Type") (cookies "session_token"))
   200))


;; serve a static HTML file
(route app "/static-file" (methods GET))
(define (static-file params headers cookies)
  (serve-static-file "example.html"))


;; serve a templated html file
(route app "/static-file" (methods GET))
(define (static-template params headers cookies)
  (serve-template-file "example.html"
                       (list (list "a" "a_value")
                             (list "b's" (list 1 2 3)))))

;; run the application
(run-web-sourcery-app app
                      #:port 9000
                      #:cors? #true
                      #:public? #false)

