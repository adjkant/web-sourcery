#lang racket

(provide
 create-header-getter
 create-cookie-getter)

(require web-server/servlet
         "../data-defs.rkt"
         "../utils/basics.rkt")

(module+ test (require "../utils/testing.rkt"))

;; [List-of Header] -> [String -> [Maybe String]]
;; create a function that searches for headers in the given request by string name
(define ((create-header-getter headers) f)
  (define matching-headers (filter (λ (h) (string=? (ws-header-field h) f)) headers))
  (when/f (cons? matching-headers)
          (ws-header-value (first matching-headers))))

(module+ test
  (check-equal? ((create-header-getter HEADERS-1) (ws-header-field HEADER-1))
                (ws-header-value HEADER-1))
  (check-equal? ((create-header-getter HEADERS-1) (ws-header-field HEADER-2))
                (ws-header-value HEADER-2))
  (check-equal? ((create-header-getter HEADERS-1) (ws-header-field HEADER-3))
                (ws-header-value HEADER-3))
  (check-false ((create-header-getter HEADERS-1) "nonexistant header")))

;; [List-of Cookie] -> [String -> [Maybe String]]
;; create a function that searches for cookies in the given request by string name
(define ((create-cookie-getter cookies) n)
  (define matching-cookies (filter (λ (c) (string=? (ws-cookie-name c) n)) cookies))
  (when/f (cons? matching-cookies)
          (ws-cookie-value (first matching-cookies))))

(module+ test
  (check-equal? ((create-cookie-getter COOKIES-1) (ws-cookie-name COOKIE-1))
                (ws-cookie-value COOKIE-1))
  (check-equal? ((create-cookie-getter COOKIES-1) (ws-cookie-name COOKIE-2))
                (ws-cookie-value COOKIE-2))
  (check-equal? ((create-cookie-getter COOKIES-1) (ws-cookie-name COOKIE-3))
                (ws-cookie-value COOKIE-3))
  (check-false ((create-cookie-getter COOKIES-1) "nonexistant cookie")))

