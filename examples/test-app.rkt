#lang racket

(require "../web-sourcery/main.rkt")
(require sql-sourcery)

;; Specify a Database
(sourcery-db "test-app.db")

;; Create Sourcery Structures
(sourcery-struct session [(path STRING)])
(sourcery-struct data [(field STRING) (timestamp INTEGER)])

(struct basic-structure [here some data] #:transparent)
(struct unserializable [struct] #:transparent)
#;(struct sub basic-structure [])

#;(json-serializer-struct sub)

(define basic-structure-serializer (json-serializer-struct basic-structure))

(define data-custom-serializer
  (json-serializer-obj basic-structure?
                       (some      basic-structure-some)
                       (data      basic-structure-data)))
#;(define data-custom-serializer-2
    (json-serializer-obj string? #;data?
                         (field      data-field)
                         (timestamp  data-timestamp)))

#;(define session-serializer (json-serializer-sourcery-struct session))

(define serializers (list basic-structure-serializer #;session-serializer data-custom-serializer))

;; Define an application
(define-web-sourcery-app app)


;; Define Routes
;; ---------------------------------------------------

(define-route [app "" [GET]] -> TEXT
  (response (session-path (session-create "blank GET")) 201-CREATED))

(define-route [app "" [POST]] -> TEXT
  (response (session-path (session-create "blank POST")) 201-CREATED))

(define-route [app "/json-output-basic" [GET]] -> JSON
  (response (json-obj (json-kv 'values (list 1 2 3))) 200-OK))

(define-route [app "/json-output-struct" [GET]] -> JSON
  (response (basic-structure 1 2 3) 200-OK))

(define-route [app "/json-output-unserializable" [GET]] -> JSON
  (response (unserializable 1) 200-OK))

(define-route [app "/json-output-invalid-type-match" [GET]] -> TEXT
  (response (list 1 2 3) 200-OK))

(define-route [app "/all-methods" [GET POST PUT DELETE]] -> TEXT
  (response (session-path
             (session-create
              (cond
                [(GET? method) "all methods GET"]
                [(POST? method) "all methods POST"]
                [(PUT? method) "all methods PUT"]
                [(DELETE? method) "all methods DELETE"]
                [else "no matching method"])))
            201-CREATED))

(define-route [app "/example" [GET]] -> TEXT
  (response (session-path (session-create "example")) 201-CREATED))


(define-route [app "/data" [GET]] -> TEXT
  (begin
    (session-path (session-create "/data"))
    (response
     (for/fold ((cur "")) ((s (sourcery-load session)))
       (string-append (session-path s) "<br>" cur))
     201-CREATED)))

(define-route [app "/accept-json-from-anywhere" [GET POST]] -> JSON
  (response (json-obj (json-kv 'value req-json)
                      (json-kv 'source (symbol->string json-source))
                      (json-kv 'num-files (length files)))
            200-OK))

#;(define-route [app "/return-cookie(and-header)" [GET]] -> TEXT
    (with-headers (with-cookies (response "" 200-OK) (cookie "c" "d")) (header "a" "b")))

#;(define-route [app "/return-cookie(and-header)-alt" [GET]] -> TEXT
    (with-headers
        (with-cookies
            (response "" 200-OK)
          (list (cookie "c" "d")))
      (list (header "a" "b"))))

(define-route [app "/<int:param-num>" [GET]] -> TEXT
  (response (session-path (session-create (string-append "Matched an int: "
                                                         (number->string param-num))))
            201-CREATED))

(define-route [app "/<string:param-string>" [GET]] -> TEXT
  (response (session-path (session-create (string-append "Matched a string: " param-string)))
            201-CREATED))

(define-route [app "/query-param-value-x" [GET]] -> TEXT
  (response (session-path (session-create (format "query param x value: ~a" (query-params "x"))))
            201-CREATED))

(define-route [app "/header/<string:header-field>" [GET]] -> TEXT
  (response (if (headers header-field)
                (headers header-field)
                "No Matching Header")
            200-OK))

(define-route [app "/cookie/<string:cookie-name>" [GET]] -> TEXT
  (response (if (cookies cookie-name)
                (cookies cookie-name)
                "No Matching Cookie")
            200-OK))

(define-route [app "/error-bad-return-type" [GET]] -> TEXT
  (response #false 200-OK))

(define-route [app "/custom-response-code" [GET]] -> TEXT
  (response "I'm a little lambda short and sweet" (custom-status 250 "Little Lambda")))

;; ---------------------------------------------------

(module+ test
  (with-app-and-serializers app (list (json-serializer-struct basic-structure))
    (check-request [GET "/"] -> [TEXT "blank GET" 201-CREATED])
    (check-request [POST "/"] -> [TEXT "blank POST" 201-CREATED])
    (check-request [GET "/json-output-struct"] -> [JSON (basic-structure 1 2 3) 200-OK])
    (check-request [GET "/json-output-invalid-type-match"] ->
                   [TEXT "Internal WebSoucery Error: route handler did not return a valid response"
                         500-INTERNAL-ERROR])
    (check-request [GET "/json-output-unserializable"] ->
                   [TEXT "Internal WebSoucery Error: route handler did not return a valid response"
                         500-INTERNAL-ERROR]) ;; TODO better error message
    (check-request [GET "/all-methods"] -> [TEXT "all methods GET" 201-CREATED])
    (check-request [POST "/all-methods"] -> [TEXT "all methods POST" 201-CREATED])
    (check-request [PUT "/all-methods"] -> [TEXT "all methods PUT" 201-CREATED])
    (check-request [DELETE "/all-methods"] -> [TEXT "all methods DELETE" 201-CREATED])
    (check-request [GET "/example"] -> [TEXT "example" 201-CREATED])
    (check-request [GET "/foo"] -> [TEXT "Matched a string: foo" 201-CREATED])
    (check-request [GET "/bar"] -> [TEXT "Matched a string: bar" 201-CREATED])
    (check-request [GET "/1"] ->
                   [TEXT "Matched an int: 1" 201-CREATED])
    (check-request [GET "/-5000"] -> [TEXT "Matched an int: -5000" 201-CREATED])
    (check-request [GET "/query-param-value-x" #:query-params (list (query-param "x" "1"))] ->
                   [TEXT "query param x value: 1" 201-CREATED])
    (check-request [GET "/foo/bar"] ->
                   [TEXT "WebSoucery: No Matching Route - 404 TODO" 404-NOT-FOUND])
    (check-request [GET "/cookie/a"
                        #:cookies (list (cookie "a" "b"))]
                   ->
                   [TEXT "b" 200-OK])
    (check-request [GET "/cookie/c"
                        #:cookies (list (cookie "a" "b"))]
                   ->
                   [TEXT "No Matching Cookie" 200-OK])
    (check-request [GET "/header/a"
                        #:headers (list (header "a" "b"))]
                   ->
                   [TEXT "b" 200-OK])
    (check-request [GET "/header/c"
                        #:headers (list (header "a" "b"))]
                   ->
                   [TEXT "No Matching Header" 200-OK])
    (check-request [GET "/custom-response-code"] ->
                   [TEXT "I'm a little lambda short and sweet" (custom-status 250 "Little Lambda")])

    #;(check-request [] -> [])
    #;(check-request [GET "/"] -> [JSON basic-structure 201-CREATED])))

;; Run Application from a custom port
#;(run-web-sourcery-app app
                        #:json-serializers serializers
                        #:cors? #t
                        #:port 1000)