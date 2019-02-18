#lang racket

(provide string->request-path
         best-matching-route
         parse-path-args)

(require "../data-defs.rkt"
         "../utils/basics.rkt")

(module+ test (require "../utils/testing.rkt"))

;; ---------------------------------------------------------------------------------------------------
;; RequestPath Generation


;; String -> RequestPath
;; convert a string into a list of reuqest path parts
(define (string->request-path path)
  (map string->request-path-part (string-split path "/")))

(module+ test
  (check-equal? (string->request-path "hello/world") REQ-PATH-1)
  (check-equal? (string->request-path "/hello/world/a/bit/longer") REQ-PATH-2)
  (check-equal? (string->request-path "1") REQ-PATH-4)
  (check-equal? (string->request-path "1/2/3") REQ-PATH-5)
  (check-equal? (string->request-path "-1") REQ-PATH-6))
  

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


;; ---------------------------------------------------------------------------------------------------
;; Request Argument Parsing


;; RequestPath PathTemplate -> [List-of RouteArg]
;; parse the given request path string according to the route
;; Invariants:
;; - request path and path template produce no #false match results when checked with
(define (parse-path-args req-path path-temp)
  (filter (compose not false?) (map parse-path-arg req-path path-temp)))

(module+ test
  (check-equal? (parse-path-args (string->request-path "/1/2/3")
                                 PATH-TEMP-6)
                (list 1 2 3))
  (check-equal? (parse-path-args (list REQ-PART-3 REQ-PART-11 REQ-PART-12)
                                 (list "a" "b" ROUTE-PARAM-STR))
                (list "c"))
  (check-equal? (parse-path-args (list REQ-PART-3 REQ-PART-11 REQ-PART-12)
                                 (list "a" "b" "c"))
                '()))

;; RequestPathPart PathPart -> [Maybe RouteArg]
;; parse a request path with the given PathPart, returning false if not a route paramter
(define (parse-path-arg req-path-part path-part)
  (when/f (ws-route-param? path-part)
          (parse-param (ws-req-path-part-string req-path-part)
                       (ws-route-param-type path-part))))

(module+ test
  (check-equal? (parse-path-arg REQ-PART-7 ROUTE-PARAM-INT) 1)
  (check-equal? (parse-path-arg REQ-PART-7 ROUTE-PARAM-STR) "1")
  (check-equal? (parse-path-arg REQ-PART-1 ROUTE-PARAM-STR) "hello")
  (check-equal? (parse-path-arg REQ-PART-7 "a") #false))


;; String PathParamType -> RouteArg
;; parse a given string to a racket value (RouteArg) corresponding to the give PathParamType
(define (parse-param param-string type)
  (cond
    [(eq? 'int type)
     (define param-int (string->number param-string))
     (if (number? param-int)
         param-int
         (error (format "Invalid route param arg type for %s: expected %s" param-string "int")))]
    [(eq? 'string type) param-string]))

(module+ test
  (check-equal? (parse-param "1" 'int) 1)
  (check-equal? (parse-param "1" 'string) "1")
  (check-equal? (parse-param "hello" 'string) "hello")
  (check-error (parse-param "hello" 'int)))


;; ---------------------------------------------------------------------------------------------------
;; Route Matching


#;(for*/list
      ([r same-length-routes]
       [mr (in-value (make-matched-route req-path r))]
       #:unless (member? #false (second mr)))
    mr)
;; RequestPath [List-of Route] -> [Maybe Route]
;; find the best matching route for the given RequestPath
(define (best-matching-route req-path routes)
  (define req-path-len (length req-path))
  (define same-length-routes
    (filter (λ (r) (= req-path-len (length (ws-route-path-temp r))))
            routes))
  (define matched-results
    (map (λ (r) (make-matched-route req-path r))
         same-length-routes))
  (define basic-matching-routes
    (filter (λ (mr) (not (member? #false (ws-matched-route-results mr))))
            matched-results))
  (select-preferred-route basic-matching-routes))


;; [List-of MatchedRoute] -> [Maybe Route]
;; Using the routing rules and given routes that basic match to their match sets, select the
;; preferred route
;; Invariants:
;; - All [List-of PathPartMatchResult] and PathTemplate are of the same length
;; - No PathPartMatchResult are #false
;; - All [List-of MatchedRoute] are unique (TODO on caller)
(define (select-preferred-route mrs)
  (or (when/f (empty? (rest mrs)) (ws-matched-route-full (first mrs))) ;; found a match!
      (next-preferred-if-nonempty (filter (create-match-filter 'exact #false)  mrs))
      (next-preferred-if-nonempty (filter (create-match-filter 'param 'int)    mrs))
      (next-preferred-if-nonempty (filter (create-match-filter 'param 'string) mrs))))

;; [List-of MatchedRoute] -> [Maybe Route]
;; generate next step of selecting a preferred route if the given matches are nonempty
(define (next-preferred-if-nonempty rs)
  (when/f (cons? rs)
          (select-preferred-route (map trim-matched-route rs))))

(module+ test
  (check-false (next-preferred-if-nonempty '()))
  (check-equal? (ws-route-path-temp (select-preferred-route MATCHED-ROUTES-1)) PATH-TEMP-1))

;; PathPartMatchResult [Maybe PathParamType] -> [MatchResult -> Boolean]
;; create a boolean filter function to select MatchResults with only
;; the given PathPartMatchResult and PathParamType in the first match result part
(define ((create-match-filter part-match-res path-param-type) mr)
  (and (eq? part-match-res (first (ws-matched-route-results mr)))
       (if (false? path-param-type)
           #true
           (eq? path-param-type (ws-route-param-type (first (ws-matched-route-parts mr)))))))

(module+ test
  (check-true ((create-match-filter 'exact #false) MATCHED-ROUTE-1))
  (check-false ((create-match-filter 'exact #false) MATCHED-ROUTE-2))
  (check-true ((create-match-filter 'param 'string) MATCHED-ROUTE-3))
  (check-false ((create-match-filter 'param 'int) MATCHED-ROUTE-3)))


;; ---------------------------------------------------------------------------------------------------
;; MatchedRoute Creation / Manipulation


;; RequestPath Route -> MatchedRoute
;; compare each path part to the path-temp and return a list of the results in order along with the
;; original route in a list
;; Invariants:
;; - request path and the route's path template are the same length
(define (make-matched-route req-path route)
  (ws-matched-route route
                    (ws-route-path-temp route)
                    (map req-part-match-route-temp-part-res req-path (ws-route-path-temp route))))

(module+ test
  (check-equal? (ws-matched-route-results (make-matched-route REQ-PATH-1 ROUTE-1))
                (list 'exact 'exact))
  (check-equal? (ws-matched-route-results (make-matched-route REQ-PATH-2 ROUTE-2))
                (list 'exact 'exact 'exact 'exact 'exact))
  (check-equal? (ws-matched-route-results (make-matched-route REQ-PATH-1 ROUTE-5))
                (list 'param #false)))


;; ReqestPathPart PathPart -> PathPartMatchResult
;; Compare two path parts (request and template) and return a result to tell
;; if it was exact, a parameter, or did not match
(define (req-part-match-route-temp-part-res req-part temp-part)
  (cond
    [(string? temp-part)
     (when/f (string=? (ws-req-path-part-string req-part) temp-part)
       'exact)]
    [(ws-route-param? temp-part)
     (when/f (member? (ws-route-param-type temp-part) (ws-req-path-part-types req-part))
       'param)]))

(module+ test
  (check-equal? (req-part-match-route-temp-part-res REQ-PART-1 "hello") 'exact)
  (check-equal? (req-part-match-route-temp-part-res REQ-PART-1 "a") #false)
  (check-equal? (req-part-match-route-temp-part-res REQ-PART-1 ROUTE-PARAM-STR) 'param)
  (check-equal? (req-part-match-route-temp-part-res REQ-PART-1 ROUTE-PARAM-INT) #false)
  (check-equal? (req-part-match-route-temp-part-res REQ-PART-8 ROUTE-PARAM-INT) 'param))


;; MatchedRoute -> MatchedRoute
;; Trim both the path-temp and the PathPartMatchResult by one item
(define (trim-matched-route mr)
  (ws-matched-route (ws-matched-route-full mr)
                    (rest (ws-matched-route-parts mr))
                    (rest (ws-matched-route-results mr))))

(module+ test
  (check-equal? (ws-matched-route-parts (trim-matched-route MATCHED-ROUTE-1))   (list "world"))
  (check-equal? (ws-matched-route-results (trim-matched-route MATCHED-ROUTE-1)) '(exact))
  (check-equal? (ws-matched-route-parts (trim-matched-route MATCHED-ROUTE-3))   '())
  (check-equal? (ws-matched-route-results (trim-matched-route MATCHED-ROUTE-3)) '()))

  
  
    
