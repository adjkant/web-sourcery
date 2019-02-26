#lang racket

(provide define-route
         handle-any-request)

(require web-server/servlet
         "path-template.rkt"
         "request-matching.rkt"
         "handler-inputs.rkt"
         "../data-defs.rkt"
         "../data-conversion.rkt"
         "../utils/basics.rkt")

(require (for-syntax syntax/parse
                     racket/syntax
                     "path-template.rkt"
                     "../utils/basics.rkt"))

(module+ test (require "../utils/testing.rkt"))

(define RESPONSE-404-ROUTE-NOT-FOUND
  (response/full 404
                 #"Not Found"
                 (current-seconds)
                 TEXT/HTML-MIME-TYPE
                 '()
                 (list (string->bytes/utf-8 "No Matching Route - 404 TODO"))))

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
        (string-append "invalid path template syntax in \"" (syntax->datum #'path) "\"")))

     (when (duplicates? param-names)
       (raise-syntax-error
        'define-route
        (string-append "duplicate path parameter names in \"" (syntax->datum #'path) "\"")))

     
     
     #`(set! app-name
             (cons (ws-route (string->path-template path)
                             (lambda #,handler-inputs route-body))
                   app-name))]))

(module+ test
  (check-compile-error 
     (define-route [app "/<int:param>/<string:param>"]
       (session-path (session-create "blank")))))

;; web-server/http/request WSApp -> web-server/http/response
;; Handle any request to the server and return an apropriate response
(define (handle-any-request req app)
  (define internal-req (request->ws-request req))
  (define matching-route (match-request-to-route internal-req app))
  (if matching-route
      (create-response (call-route-with-req matching-route internal-req))
      RESPONSE-404-ROUTE-NOT-FOUND))

;; String -> web-server/http/response
;; Create a response from the given string
(define (create-response str)
  (response/full 200
                 #"Ok"
                 (current-seconds)
                 TEXT/HTML-MIME-TYPE
                 '()
                 (list (string->bytes/utf-8 str))))

;; Request WSApp -> String
;; Route a request to the apropriate handler and return the result
(define (match-request-to-route req app)
  (best-matching-route (ws-request-path req) app))

(module+ test
  )

;; TODO extensive testing on this function
;; TODO check validity of route and trim empty string at end of routes
;; User Note: "Trailing slashes are ignored during routing"
   


;; Route Request -> String
;; Call the given route's handler with the given request info
;; or create a 404 response if no route is given
(define (call-route-with-req route req)
  (apply (ws-route-handler route)
         (append (parse-path-args (ws-request-path req) (ws-route-path-temp route))
                 (list (create-header-getter (ws-request-headers req))
                       (create-cookie-getter (ws-request-cookies req))))))


(module+ test
  )
