#lang racket

(provide
 create-header-getter
 create-cookie-getter)

(require web-server/servlet
         "../data-defs.rkt"
         "../utils/basics.rkt")


;; TODO testing - need request generation

;; Request -> [String -> [Maybe String]]
;; create a function that searches for headers in the given request by string name
(define ((create-header-getter req) f)
  (define headers (request-headers/raw req))
  (define ws-headers (map header->ws-header headers))
  (define matching-headers (filter (λ (h) (string=? (ws-header-field h) f)) ws-headers))
  (when/f (cons? matching-headers)
          (ws-header-value (first matching-headers))))


;; web-server/http/request-structs/header -> Header
;; convert a header struct to a ws-header
(define (header->ws-header h)
  (ws-header (bytes->string/utf-8 (header-field h))
             (bytes->string/utf-8 (header-value h))))


;; Request -> [String -> [Maybe String]]
;; create a function that searches for cookies in the given request by string name
(define ((create-cookie-getter req) n)
  (define cookies (request-cookies req))
  (define ws-cookies (map client-cookie->ws-cookie cookies))
  (define matching-cookies (filter (λ (c) (string=? (ws-cookie-name c) n)) ws-cookies))
  (when/f (cons? matching-cookies)
          (ws-cookie-value (first matching-cookies))))


;; client-cookie -> Cookie
;; convert a client-cookie to a ws-cookie
(define (client-cookie->ws-cookie c)
  (ws-cookie (client-cookie-name c)
             (client-cookie-value c)))


