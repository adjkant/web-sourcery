#lang racket

(provide define-route
         match-request-to-route)

(require web-server/servlet
         "path-template.rkt"
         "request-matching.rkt"
         "../data-defs.rkt"
         "../utils/basics.rkt")
(require (for-syntax syntax/parse
                     racket/syntax
                     "path-template.rkt"))

;; Top Level Route Definition
(define-syntax define-route
  (syntax-parser
    [(_ [app-name:id path:string] route-body)
     (define path-template (string->path-template (syntax->datum #'path)))
     (define param-names (get-param-names path-template))
     (define param-ids (map (λ (n) (format-id #'path "~a" n)) param-names))
     (unless path-template
       (raise-syntax-error
        'define-route
        (string-append "Invalid path template syntax: \"" (syntax->datum #'path) "\"")))
     #`(set! app-name
             (cons (ws-route (string->path-template path)
                             (lambda #,param-ids route-body))
                   app-name))]))


;; Request WSApp -> String
;; Route a request to the apropriate handler and return the result
(define (match-request-to-route req app)
  (define path-string (first (map path/param-path (url-path (request-uri req)))))
  (define req-path (string->request-path path-string))
  (define matched-route (best-matching-route req-path app))
  (if matched-route
      (apply (ws-route-handler matched-route)
             (parse-path-args req-path (ws-route-path-temp matched-route)))
      "No Matching Route - 404 TODO"))



