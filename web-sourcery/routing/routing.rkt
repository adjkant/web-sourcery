#lang racket

(provide define-route
         handle-any-request)

(require web-server/servlet
         "path-template.rkt"
         "request-matching.rkt"
         "handler-inputs.rkt"
         "../data-defs.rkt"
         "../data-conversion.rkt"
         "../utils/basics.rkt"
         "../http/methods.rkt")

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

(define RESPONSE-400-BAD-REQUEST-NO-STRING-RETURN
  (response/full 400
                 #"Bad Request"
                 (current-seconds)
                 TEXT/HTML-MIME-TYPE
                 '()
                 (list (string->bytes/utf-8 "Route handler did not return a string"))))

;; Top Level Route Definition
;; TODO contacts on methods
(define-syntax define-route
  (syntax-parser
    [(_ [app-name:id path:string [method ...]] route-body)
     (define path-template (string->path-template (syntax->datum #'path)))
     (define param-names (get-param-names path-template))
     (define param-ids (map (λ (n) (format-id #'path "~a" n)) param-names))
     (define trailing-ids (list (format-id #'path "~a" "method")
                                (format-id #'path "~a" "query-params")
                                (format-id #'path "~a" "headers")
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
             (add-route path
                        (ws-route (string->path-template path)
                                  (list method ...)
                                  (lambda #,handler-inputs route-body))
                        app-name))]))

(module+ test
  (check-compile-error 
   (define-route [app "/<int:param>/<string:param>" [GET]]
     (session-path (session-create "blank")))))

;; String Route WSApp
;; adds a route while throwing an error if the route path would be a duplicate
(define (add-route route-string route app)
  (define added-route-path-temp (ws-route-path-temp route))
  (define added-route-methods (ws-route-methods route))
  (define duplicate-route?
    (ormap
     (λ (r)
       (and (analagous-path-template? (ws-route-path-temp r) added-route-path-temp)
            (map (λ (m) (member? m (ws-route-methods r))) added-route-methods))) 
     app)) 
  (if (not duplicate-route?)
      (cons route app)
      (error 'duplicate-route (format "duplicate route at: ~s [~s]"
                                      route-string
                                      (foldr (λ (ms acc) (string-append acc " " ms))
                                             (method->string (first added-route-methods))
                                             (map method->string (rest added-route-methods)))))))


;; web-server/http/request WSApp -> web-server/http/response
;; Handle any request to the server and return an apropriate response
(define (handle-any-request req app)
  (define internal-req (request->ws-request req))
  (define matching-route (best-matching-route internal-req app))
  (if matching-route
      (create-response (call-route-with-req matching-route internal-req))
      RESPONSE-404-ROUTE-NOT-FOUND))

;; String -> web-server/http/response
;; Create a response from the given string
(define (create-response str)
  (if (string? str)
      (response/full 200
                     #"Ok"
                     (current-seconds)
                     TEXT/HTML-MIME-TYPE
                     '()
                     (list (string->bytes/utf-8 str)))
      RESPONSE-400-BAD-REQUEST-NO-STRING-RETURN))


;; Route Request -> String
;; Call the given route's handler with the given request info
;; or create a 404 response if no route is given
(define (call-route-with-req route req)
  (define path-params (parse-path-args (ws-request-path req) (ws-route-path-temp route)))
  (define handler-request-inputs
    (list (ws-request-method req)
          (create-query-param-getter (ws-request-query-params req))
          (create-header-getter (ws-request-headers req))
          (create-cookie-getter (ws-request-cookies req))))
  (apply (ws-route-handler route)
         (append path-params handler-request-inputs)))


(module+ test
  )
;; TODO testing
