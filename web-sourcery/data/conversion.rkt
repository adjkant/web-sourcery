#lang racket

(provide
 request->ws-request
 internal-response->external-response
 response-error-code->response
 response-error-code->string
 response-error-code->status
 strings->request-path
 strings->static-folder-route)

(require web-server/servlet
         web-server/private/mime-types
         json
         json-sourcery
         "defs.rkt"
         "../response/response.rkt"
         "../response/preset.rkt"
         "../http/status-codes.rkt"
         "../utils/basics.rkt")

(module+ test (require "../utils/testing.rkt"))

;; TODO testing - need request generation

;; web-server/http/request -> Request
;; Convert to an internal request representation
(define (request->ws-request req)
  (define ws-headers (request->ws-headers req))
  (ws-request
   (request->ws-method req)
   (strings->request-path (trim-trailing-empty-string (map path/param-path
                                                           (url-path (request-uri req)))))
   (request->ws-query-params req)
   ws-headers
   (request->ws-cookies req)
   (request->json req ws-headers)
   (request->data-source req ws-headers)
   (request->files req)))

;; Response [Maybe Route] [List-of JSONSerializer] -> web-server/http/response
;; Create an external response for a Response that passes the valid-response? predicate
(define (internal-response->external-response internal-response matching-route serializers)
  (cond
    [(ws-response? internal-response)
     (ws-response->external-response internal-response
                                     (ws-route-response-type matching-route)
                                     serializers)]
    [(string? internal-response)
     (file-path->external-file-response internal-response)]
    [(natural? internal-response)
     (response-error-code->response internal-response)]))

;; DataResponse ResponseType [List-of JSONSerializer] -> web-server/http/response
;; Create an external response for a ws-response with the given type and serializers
;; invariant: all response data can be serialized
(define (ws-response->external-response r t serializers)
  (define response-data-bytes (ws-response-data->bytes (ws-response-data r) serializers))
  (response/full (ws-status-code (ws-response-status r))
                 (string->bytes/utf-8 (ws-status-description (ws-response-status r)))
                 (current-seconds)
                 (ws-response-type->response-type t)
                 (append (map ws-header->header (ws-response-headers r))
                         (map (compose cookie->header ws-cookie->cookie) (ws-response-cookies r))
                         ALL-RESPONSE-HEADERS)
                 (list response-data-bytes)))

;; FileResponse -> web-server/http/response
;; create a file response for the given file path
(define (file-path->external-file-response file-path)
  (response/full 200
                 #"Ok"
                 (current-seconds)
                 (file->mime-type file-path)
                 ALL-RESPONSE-HEADERS
                 (list (file->bytes file-path))))

;; FileResponse -> Bytes
;; get the mime type for the given file
(define (file->mime-type file-path)
  (define file-ext (string->symbol (last (string-split file-path "."))))
  (define mime-type-lookup-table (read-mime-types "../mime.types"))
  (hash-ref mime-type-lookup-table file-ext))

(define (string-checker s)
  (λ (gs) (string=? s gs)))
  

;; ResponseErrorCode -> web-server/http/response
;; convert an internal handler error code into an external response
(define (response-error-code->response error-code)
  (define response-status (response-error-code->status error-code))
  (define error-string (response-error-code->string error-code))
  (response/full (ws-status-code response-status)
                 (string->bytes/utf-8 (ws-status-description response-status))
                 (current-seconds)
                 TEXT/HTML-MIME-TYPE
                 ALL-RESPONSE-HEADERS
                 (list (string->bytes/utf-8 error-string))))

;; ResponseErrorCode -> String
;; convert an internal handler error code into an external response
(define (response-error-code->string error-code)
  (cond
    [(= error-code 404) DEFAULT-RESPONSE-404-ROUTE-NOT-FOUND-MESSAGE]
    [(= error-code 500) RESPONSE-500-INVALID-RESPONSE-MESSAGE]))

;; ResponseErrorCode -> StatusCode
;; convert an internal handler error code into an external response
(define (response-error-code->status error-code)
  (cond
    [(= error-code 404) 404-NOT-FOUND]
    [(= error-code 500) 500-INTERNAL-ERROR]))

;; ResponseType -> Bytes
(define (ws-response-type->response-type rt)
  (cond [(symbol=? rt 'TEXT) TEXT/HTML-MIME-TYPE]
        [(symbol=? rt 'JSON) JSON-MIME-TYPE]))

;; Any -> Bytes
(define (ws-response-data->bytes rd serializers)
  (cond [(string? rd) (string->bytes/utf-8 rd)]
        [else (jsexpr->bytes (serialize-json rd serializers))]))

;; Header -> web-server/http/header
;; convert an internal cookie to an external cookie
(define (ws-header->header h)
  (header (string->bytes/utf-8 (ws-header-field h))
          (string->bytes/utf-8 (ws-header-value h))))

;; Cookie -> web-server/http/cookie
;; convert an internal cookie to an external cookie
(define (ws-cookie->cookie c)
  (make-cookie (ws-cookie-name c) (ws-cookie-value c)))

;; String String -> StaticFolderRoute
;; convert a given app path and serving path into a StaticRoute
(define (strings->static-folder-route app-path serve-path)
  (define app-path-strings (trim-trailing-empty-string (string-split app-path "/")))
  (define serve-path-strings (trim-trailing-empty-string (string-split serve-path "/")))
  (ws-static-folder-route app-path-strings serve-path-strings))

;; String -> RequestPath
;; convert a string into a list of reuqest path parts
(define (strings->request-path path-part-strings)
  (map string->request-path-part path-part-strings))

(module+ test
  (check-equal? (strings->request-path (list "hello" "world")) REQ-PATH-1)
  (check-equal? (strings->request-path (string-split "/hello/world/a/bit/longer" "/")) REQ-PATH-2)
  (check-equal? (strings->request-path (list "1")) REQ-PATH-4)
  (check-equal? (strings->request-path (list "1" "2" "3")) REQ-PATH-5)
  (check-equal? (strings->request-path (list "-1")) REQ-PATH-6))
  

;; String -> RequestPathPart
;; convert a single string into a request path part
(define (string->request-path-part part)
  (if (and (string->number part) (integer? (string->number part)))
      (ws-req-path-part part '(int string))
      (ws-req-path-part part '(string))))

(module+ test
  (check-equal? (string->request-path-part "test") (ws-req-path-part "test" '(string)))
  (check-equal? (string->request-path-part "1") (ws-req-path-part "1" '(int string)))
  (check-equal? (string->request-path-part "-1") (ws-req-path-part "-1" '(int string))))


;; web-server/http/request -> Method
;; Get a Method from a request
(define (request->ws-method req)
  (define method-symbol (string->symbol (string-upcase (bytes->string/utf-8 (request-method req)))))
  (ws-method method-symbol))


;; web-server/http/request -> [List-of QueryParam]
;; Get a list of query params from a request
(define (request->ws-query-params req)
  (map query-param->ws-query-param (url-query (request-uri req))))

;; [Pair Symbol [Maybe String]] -> QueryParam
;; Get a structured QueryParam from a query param 
(define (query-param->ws-query-param qp)
  (ws-query-param (symbol->string (car qp)) (cdr qp)))


;; web-server/http/request -> [List-of Cookie]
;; Get a list of cookies from a request
(define (request->ws-cookies req)
  (map client-cookie->ws-cookie (request-cookies req)))


;; client-cookie -> Cookie
;; convert a client-cookie to a ws-cookie
(define (client-cookie->ws-cookie c)
  (ws-cookie (client-cookie-name c)
             (client-cookie-value c)))


;; web-server/http/request -> [List-of Header]
;; Get a list of headers from a request
(define (request->ws-headers req)
  (map header->ws-header (request-headers/raw req)))


;; web-server/http/request-structs/header -> Header
;; convert a header struct to a ws-header
(define (header->ws-header h)
  (ws-header (bytes->string/utf-8 (header-field h))
             (bytes->string/utf-8 (header-value h))))

;; web-server/http/request [List-of Header] -> [Maybe jsexpr]
;; get JSON from a requests's post data, url enconced variables, or form data
(define (request->json req headers)
  (define req-bindings (filter binding:form? (force (request-bindings/raw-promise req))))
  (define req-post-data (request-post-data/raw req))
  (define req-post-string (if req-post-data (bytes->string/utf-8 req-post-data) ""))
  (define has-json-header
    (not (empty? (filter (λ (h)
                           (and (string=? (ws-header-field h) "Content-Type")
                                (string=? (ws-header-value h) "application/json")))
                         headers))))
  (cond [(empty? req-bindings)
         (when/f (and (jsexpr? req-post-string) has-json-header)
                 (string->jsexpr req-post-string))]
        [else (request-bindings->json/f req-bindings)]))

;; [List-of web-server/http/request-structs/binding] -> [Maybe jsexpr]
;; turn a list of bindings into JSON or #f if no bindings exist
(define (request-bindings->json/f bindings)
  (when/f (cons? bindings)
          (json-obj (map (λ (b) (json-kv (string->symbol (bytes->string/utf-8 (binding-id b)))
                                         (bytes->string/utf-8 (binding:form-value b))))
                         bindings))))
        
;; web-server/http/request [List-of Header] -> JSONSource
;; find the source of the converted JSON for a given request
(define (request->data-source req headers)
  (define req-bindings (filter binding:form? (force (request-bindings/raw-promise req))))
  (define req-post-data (request-post-data/raw req))
  (define req-post-string (if req-post-data (bytes->string/utf-8 req-post-data) ""))
  (define has-json-header
    (not (empty? (filter (λ (h)
                           (and (string=? (ws-header-field h) "Content-Type")
                                (string=? (ws-header-value h) "application/json")))
                         headers))))
  (cond [(empty? req-bindings)
         (if (and (jsexpr? req-post-string) has-json-header)
             'json
             'none)]
        [(> (string-length req-post-string) 0) 'url]
        [else 'form]))


;; web-server/http/request -> [List-of File]
;; get the files out of a request and convert them to ws-files
(define (request->files req)
  (map file->ws-file (filter binding:file? (force (request-bindings/raw-promise req)))))

;; web-server/http/request-structs/binding:file -> File
;; convert a single file to a ws-file
(define (file->ws-file f)
  (define file-content-type
    (foldr (λ (h v)
             (if (string=? "Content-Type" (ws-header-field h))
                 (ws-header-value h)
                 ""))
           ""
           (map header->ws-header (binding:file-headers f))))
  (ws-file (bytes->string/utf-8 (binding-id f))
           (bytes->string/utf-8 (binding:file-filename f))
           file-content-type
           (binding:file-content f)))





