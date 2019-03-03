#lang racket

(provide define-route
         handle-any-request)

(require web-server/servlet
         "path-template.rkt"
         "request-matching.rkt"
         "handler-inputs.rkt"
         "../data/defs.rkt"
         "../data/conversion.rkt"
         "../utils/basics.rkt"
         "../http/methods.rkt"
         "../response/preset.rkt")

(require (for-syntax syntax/parse
                     racket/syntax
                     "path-template.rkt"
                     "../utils/basics.rkt"))

(module+ test (require "../utils/testing.rkt"))

;; Top Level Route Definition
;; TODO contacts on methods
(define-syntax define-route
  (syntax-parser
    [(_ [app-name:id path:string [method ...]] (~literal ->) type:id route-body)
     (define path-template (string->path-template (syntax->datum #'path)))
     (define param-names (get-param-names path-template))
     (define param-ids (map (λ (n) (format-id #'path "~a" n)) param-names))
     (define trailing-ids (list (format-id #'path "~a" "method")
                                (format-id #'path "~a" "query-params")
                                (format-id #'path "~a" "headers")
                                (format-id #'path "~a" "cookies")))
     (define handler-inputs (append param-ids trailing-ids))
     (define response-type (syntax->datum #'id))
     
     (unless path-template
       (raise-syntax-error
        'define-route
        (format "invalid path template syntax in \"~s\"" (syntax->datum #'path))))

     (when (duplicates? param-names)
       (raise-syntax-error
        'define-route
        (format "duplicate path parameter names in \"~s\"" (syntax->datum #'path))))

     (unless (member? response-type VALID-RESPONSE-TYPES)
       (raise-syntax-error
        'define-route
        (format "invalid handler response type: ~s" response-type)))
     
     
     #`(set! app-name
             (add-route path
                        (ws-route (string->path-template path)
                                  (list method ...)
                                  #,response-type
                                  (lambda #,handler-inputs route-body))
                        app-name))]))

(module+ test
  (check-compile-error 
   (define-route [app "/<int:param>/<string:param>" [GET]] -> TEXT
     (session-path (session-create "blank")))))

;; String Route WSApp
;; adds a route while throwing an error if the route path would be a duplicate
(define (add-route route-string route app)
  (define added-route-path-temp (ws-route-path-temp route))
  (define added-route-methods (ws-route-methods route))
  (define duplicate-route? (ormap (λ (r) (analagous-route? r route)) app))
  (if (not duplicate-route?)
      (cons route app)
      (error 'duplicate-route (format "at ~s [~s]"
                                      route-string
                                      (foldr (λ (ms acc) (string-append acc " " ms))
                                             (method->string (first added-route-methods))
                                             (map method->string (rest added-route-methods)))))))

;; Route Route -> Boolean
;; determine if the given routes will match over at least one request with with the same priority
(define (analagous-route? r1 r2)
  (and (analagous-path-template? (ws-route-path-temp r1) (ws-route-path-temp r2))
       (ormap (λ (m) (member? m (ws-route-methods r1))) (ws-route-methods r2))))

(module+ test
  (check-true (analagous-route? ROUTE-1 ROUTE-1))
  (check-true (analagous-route? ROUTE-4 ROUTE-7))
  (check-false (analagous-route? ROUTE-1 ROUTE-4))
  (check-false (analagous-route? ROUTE-2 ROUTE-5))
  (check-false (analagous-route? ROUTE-7 ROUTE-8)))
  

;; web-server/http/request WSApp -> web-server/http/response
;; Handle any request to the server and return an apropriate response
;; testing here should be done at top level
(define (handle-any-request req app)
  (define internal-req (request->ws-request req))
  (define matching-route (best-matching-route internal-req app))
  (if matching-route
      (ws-response->response (call-route-with-req matching-route internal-req))
      DEFAULT-RESPONSE-404-ROUTE-NOT-FOUND))

;; Route Request -> Response
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
