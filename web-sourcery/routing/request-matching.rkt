#lang racket

(provide string->request-path
         best-matching-route
         parse-path-args)

(require "ws-route.rkt"
         "../utils.rkt")

(struct ws-req-path-part [string types])
(struct ws-matched-route [full parts results])

;; RequestPath is a [List-of RequestPathPart]

;; RequestPathPart is a (ws-req-path-part String [List-of PathParamType])

;; A RouteArg is one of:
;; - Integer
;; - String

;; MatchedRoute is a (ws-matched-route Route PathTemplate [List-of PathPartMatchResult])

;; PathPartMatchResult is one of:
;; - 'exact
;; - 'param
;; - #false


;; ---------------------------------------------------------------------------------------------------
;; RequestPath Generation


;; String -> RequestPath
;; convert a string into a list of reuqest path parts
(define (string->request-path path)
  (map string->request-path-part (string-split path "/")))

;; String -> RequestPathPart
;; convert a single string into a request path part
(define (string->request-path-part part)
  (if (and (string->number part) (integer? (string->number part)))
      (ws-req-path-part part (list 'int 'string))
      (ws-req-path-part part (list 'string))))


;; ---------------------------------------------------------------------------------------------------
;; Request Argument Parsing


;; RequestPath PathTemplate -> [List-of RouteArg]
;; parse the given request path string according to the route
(define (parse-path-args req-path path-temp)
  (filter (compose not false?) (map parse-path-arg req-path path-temp)))

;; RequestPathPart PathPart -> [Maybe RouteArg]
(define (parse-path-arg req-path-part path-part)
  (cond
    [(ws-route-param? path-part) (parse-param (ws-req-path-part-string req-path-part)
                                              (ws-route-param-type path-part))]
    [else #false]))

;; String PathParamType -> RouteArg
(define (parse-param param-string type)
  (cond
    [(eq? 'int type) (string->number param-string)]
    [(eq? 'string type) param-string]))


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
  (cond
    [(cons? rs) (select-preferred-route (map trim-matched-route rs))]
    [else #false]))

;; PathPartMatchResult [Maybe PathParamType] -> [MatchResult -> Boolean]
;; create a boolean filter function to select MatchResults with only
;; the given PathPartMatchResult and PathParamType
(define ((create-match-filter exact-or-param string-or-int) mr)
  (and (eq? exact-or-param (first (ws-matched-route-results mr)))
       (if (false? string-or-int)
           #true
           (eq? string-or-int (ws-route-param-type (first (ws-matched-route-parts mr)))))))


;; ---------------------------------------------------------------------------------------------------
;; MatchedRoute Creation / Manipulation


;; RequestPath Route -> MatchedRoute
;; compare each path part to the path-temp and return a list of the results in order along with the
;; original route in a list
(define (make-matched-route req-path route)
  (ws-matched-route route
                    (ws-route-path-temp route)
                    (map req-part-match-route-temp-part-res req-path (ws-route-path-temp route))))

;; ReqestPathPart PathPart -> PathPartMatchResult
;; Compare two path parts (request and template) and return a result to tell
;; if it was exact, a parameter, or did not match
(define (req-part-match-route-temp-part-res req-part temp-part)
  (cond
    [(string? temp-part)
     (when (string=? (ws-req-path-part-string req-part) temp-part)
       'exact)]
    [(ws-route-param? temp-part)
     (when (member? (ws-route-param-type temp-part) (ws-req-path-part-types req-part))
       'param)]))


;; MatchedRoute -> MatchedRoute
;; Trim both the path-temp and the PathPartMatchResult by one item
(define (trim-matched-route mr)
  (ws-matched-route (ws-matched-route-full mr)
                    (rest (ws-matched-route-parts mr))
                    (rest (ws-matched-route-results mr))))
    
