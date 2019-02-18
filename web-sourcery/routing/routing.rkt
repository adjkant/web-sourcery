#lang racket

(provide define-route
         match-request-to-route)

(require web-server/servlet
         "path-template.rkt"
         "request-matching.rkt"
         "handler-inputs.rkt"
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
     (define trailing-ids (list (format-id #'path "~a" "headers")
                                (format-id #'path "~a" "cookies")))
     (define handler-inputs (append param-ids trailing-ids))
     (unless path-template
       (raise-syntax-error
        'define-route
        (string-append "Invalid path template syntax: \"" (syntax->datum #'path) "\"")))
     #`(set! app-name
             (cons (ws-route (string->path-template path)
                             (lambda #,handler-inputs route-body))
                   app-name))]))


;; Request WSApp -> String
;; Route a request to the apropriate handler and return the result
(define (match-request-to-route req app)
  (define req-path-strings (map path/param-path (url-path (request-uri req))))
  
  (define req-path (strings->request-path req-path-strings))
  (define matched-route (best-matching-route req-path app))
  (if matched-route
      (apply (ws-route-handler matched-route)
             (append (parse-path-args req-path (ws-route-path-temp matched-route))
                     (list (create-header-getter req)
                           (create-cookie-getter req))))
      "No Matching Route - 404 TODO"))

;; TODO check validity of route and trim empty string at end of routes
;; User Note: "Trailing slashes are ignored during routing"
