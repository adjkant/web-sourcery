#lang racket

(provide best-matching-static-route
         find-static-file
         best-matching-route
         parse-path-args)

(require "../data/defs.rkt"
         "../utils/basics.rkt"
         "../files/static-files.rkt")

(module+ test (require "../utils/testing.rkt"
                       "../data/conversion.rkt"))


;; ---------------------------------------------------------------------------------------------------
;; Request Argument Parsing


;; RequestPath PathTemplate -> [List-of RouteArg]
;; parse the given request path string according to the route
;; Invariants:
;; - request path and path template produce no #false match results when checked with
(define (parse-path-args req-path path-temp)
  (filter (compose not false?) (map parse-path-arg req-path path-temp)))

(module+ test
  (check-equal? (parse-path-args (strings->request-path (list "1" "2" "3"))
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
;; Static Route Matching

;; RequestPath [List-of StaticRoute] -> [Maybe StaticRoute]
;; find the best matching static route
(define (best-matching-static-route req-path static-routes)
  (define matching-static-routes (filter (λ (sr) (matching-static-route? req-path sr)) static-routes))
  (when/f (cons? matching-static-routes)
          (first matching-static-routes)))

;; RequestPath StaticRoute -> Boolean
;; Determine if a given request path is part of a static route
(define (matching-static-route? req-path sr)
  (define string-parts (map ws-req-path-part-string req-path))
  (define app-path (ws-static-folder-route-app-path sr))
  (when/f (> (length string-parts) (length app-path))
          (and (andmap string=? app-path (take string-parts (length app-path)))
               (andmap (λ (s) (not (string=? s "..")))
                       (list-tail string-parts (length app-path))))))

;; RequestPath StaticRoute -> Response
;; Look for the given file in the static route and return the full file path
;; return 404 if the file does not exist
(define (find-static-file req-path sr)
  (define string-parts (map ws-req-path-part-string req-path))
  (define app-path (ws-static-folder-route-app-path sr))
  (define requested-path-to-file (list-tail string-parts (length app-path)))
  (define full-file-path (append (ws-static-folder-route-file-path sr) requested-path-to-file))
  (define built-file-path (foldr (λ (p so-far) (string-append p "/" so-far)) "" full-file-path))
  (define full-file-path-string (string-append "/"
                                               (substring built-file-path
                                                          0
                                                          (sub1 (string-length built-file-path)))))
  (if (file-exists? full-file-path-string)
      full-file-path-string
      404))


;; ---------------------------------------------------------------------------------------------------
;; Route Matching


#;(for*/list
      ([r same-length-routes]
       [mr (in-value (make-matched-route req-path r))]
       #:unless (member? #false (second mr)))
    mr)

;; Request [List-of Route] -> [Maybe Route]
;; find the best matching route for the given Request
(define (best-matching-route req routes)
  (define req-path (ws-request-path req))
  (define req-method (ws-request-method req))
  (define req-path-len (length req-path))
  (define same-method-routes
    (filter (λ (r) (member? req-method (ws-route-methods r))) routes))
  (define same-length-routes
    (filter (λ (r) (= req-path-len (length (ws-route-path-temp r))))
            same-method-routes))
  (define matched-results
    (map (λ (r) (make-matched-route req-path r))
         same-length-routes))
  (define basic-matching-routes
    (filter (λ (mr) (not (member? #false (ws-matched-route-results mr))))
            matched-results))
  (when/f (cons? basic-matching-routes)
          (select-preferred-route basic-matching-routes)))

;; User Note: "Trailing slashes are ignored during routing"
   

;; [List-of MatchedRoute] -> [Maybe Route]
;; Using the routing rules and given routes that basic match to their match sets, select the
;; preferred route
;; Invariants:
;; - matched routes list is nonempty
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
  (when/f (and (cons? rs) (cons? (ws-matched-route-results (first rs))))
          (select-preferred-route (map trim-matched-route rs))))

(module+ test
  (check-false (next-preferred-if-nonempty (list MATCHED-ROUTE-0 MATCHED-ROUTE-0)))
  (check-false (next-preferred-if-nonempty '()))
  (check-equal? (ws-route-path-temp (select-preferred-route MATCHED-ROUTES-1)) PATH-TEMP-1))

;; PathPartMatchResult [Maybe PathParamType] -> [MatchResult -> Boolean]
;; create a boolean filter function to select MatchResults with only
;; the given PathPartMatchResult and PathParamType in the first match result part
(define ((create-match-filter part-match-res path-param-type) mr)
  (or (empty? (ws-matched-route-results mr))
      (and (eq? part-match-res (first (ws-matched-route-results mr)))
           (if (false? path-param-type)
               #true
               (eq? path-param-type (ws-route-param-type (first (ws-matched-route-parts mr))))))))

(module+ test
  (check-true ((create-match-filter 'exact #false) MATCHED-ROUTE-0))
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

  
  
    
