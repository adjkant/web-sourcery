#lang racket

(provide define-route
         handle-any-request/external
         handle-any-request/internal)

(require web-server/servlet
         "path-template.rkt"
         "request-matching.rkt"
         "handler-inputs.rkt"
         "../data/defs.rkt"
         "../data/conversion.rkt"
         "../utils/basics.rkt"
         "../http/methods.rkt"
         "../response/response.rkt")

(require (for-syntax syntax/parse
                     racket/syntax
                     "path-template.rkt"
                     "../utils/basics.rkt"
                     "../data/defs.rkt"))

(module+ test (require "../utils/testing.rkt"))

;; Top Level Route Definition
;; TODO contacts on methods
(define-syntax define-route
  (syntax-parser
    [(_ [app-name:id path:string [one-method method ...]]
        (~literal ->)
        response-type-syntax:id route-body)
     
     (define path-template (string->path-template (syntax->datum #'path)))

     (unless path-template
       (raise-syntax-error
        'define-route
        (format "invalid path template syntax in \"~s\"" (syntax->datum #'path))))
     
     (define param-names (get-param-names path-template))
     (define param-ids (map (λ (n) (format-id #'path "~a" n)) param-names))
     (define trailing-ids (list (format-id #'path "~a" "method")
                                (format-id #'path "~a" "query-params")
                                (format-id #'path "~a" "headers")
                                (format-id #'path "~a" "cookies")))
     (define handler-inputs (append param-ids trailing-ids))
     (define response-type (syntax->datum #'response-type-syntax))

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
                                  (list one-method method ...)
                                  (string->symbol #,(symbol->string response-type))
                                  (lambda #,handler-inputs route-body))
                        app-name))]))

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


;; web-server/http/request WSApp [List-of JSONSerializer] -> web-server/http/response
;; Handle any request to the server and return an apropriate response
(define (handle-any-request/external req app serializers)
  (define internal-request (request->ws-request req))
  (define matching-route (best-matching-route internal-request app))
  (define internal-response (handle-any-request/internal internal-request app serializers))
  (internal-response->external-response internal-response matching-route serializers))
  

;; Request WSApp [List-of JSONSerializer] -> Response
;; Handle any request to the server and return an apropriate response
(define (handle-any-request/internal req app serializers)
  (define matching-route (best-matching-route req app))
  (if matching-route
      (call-route-with-req matching-route req serializers)
      404))
        

;; Route Request [List-of JSONSerializer] -> Response
;; Call the given route's handler with the given request info and convert the response to an external
;; format, producing an error response if the handler does not return a proper value
(define (call-route-with-req route req serializers)
  (define response-type (ws-route-response-type route))
  (define path-params (parse-path-args (ws-request-path req) (ws-route-path-temp route)))
  (define automatic-handler-inputs (list (ws-request-method req)
                                         (create-query-param-getter (ws-request-query-params req))
                                         (create-header-getter (ws-request-headers req))
                                         (create-cookie-getter (ws-request-cookies req))))
  (define handler-inputs (append path-params automatic-handler-inputs))
  (define response (apply (ws-route-handler route) handler-inputs))
  (if (valid-response? response response-type serializers)
      response
      500))

