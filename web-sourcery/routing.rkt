#lang racket

(provide define-route
         match-request-to-route)

(require web-server/servlet)
(require (for-syntax syntax/parse
                     racket/syntax))


(struct ws-route [path-temp handler])
(struct ws-route-param [name type])
(struct ws-req-path-part [string types])

(define VALID-ROUTE-PARAM-TYPES (list "string" "int"))
(define MIN-PATH-PARAM-STRING-SIZE (string-length "<int:a>"))

;; A Route (ws-route PathTemplate RouteHandler)

;; A RouteHandler is a [??? -> String]

;; PathTemplate is a [List-of PathPart]

;; PathPart is one of:
;; - String
;; - PathParam

;; A PathParam is a (ws-route-param String PathParamType)

;; A PathParamType is one of:
;; - 'string
;; - 'int

;; RequestPath is a [List-of RequestPathPart]

;; RequestPathPart is a (ws-req-path-part String [List-of PathParamType])

;; A RouteArg is one of:
;; - Integer
;; - String

;; MatchedRoute is a (list Route [List-of PathPartMatchResult])

;; PathPartMatchResult is one of:
;; - 'exact
;; - 'param
;; - #false





;; Top Level Route Definition
;; TODO check path template syntax at compile-time
;; TODO generate and use args in the generated handler
(define-syntax define-route
  (syntax-parser
    [(_ [app-name:id path:string] route-body)
     #`(let [(path-template (string->path-template path))]
         (if path-template
             (set! app-name
                   (cons (ws-route path-template (lambda () route-body))
                         app-name))
             (error 'define-route (string-append "Invalid path template syntax: \"" path "\""))))]))


;; Request WSApp -> Bytes
;; Route a request to the apropriate handler and return the result
(define (match-request-to-route req app)
  (let* [(req-path (string->request-path (first (map path/param-path (url-path (request-uri req))))))
         (matched-route (best-matching-route req-path app))]
    (if matched-route
        (string->bytes/utf-8 ((ws-route-handler matched-route)
                              #;(parse-path-args req-path (ws-route-path-template matched-route))))
        (string->bytes/utf-8 "no matching route"))))




;; String -> [Maybe PathTemplate]
;; convert a string into a PathTemplate or #false if any part is invalid
(define (string->path-template path)
  (let* [(split-path (string-split path "/"))]
    (if-else-false (andmap valid-path-part? split-path)
                   (map (λ (path-part)
                          (if (valid-path-param? path-part)
                              (string->path-param path-part)
                              path-part))
                        split-path))))


;; String -> Boolean
;; determine if the string is a valid PathPart
(define (valid-path-part? path-part)
  (not (or (and (path-param-shape? path-part)
                (not (valid-path-param? path-part))))))



;; String -> Boolean
;; Determine if the given PathPart string is in a valid PathParam form
(define (path-param-shape? path-part-string)
  (if-else-false (>= (string-length path-part-string) MIN-PATH-PARAM-STRING-SIZE)
                 (let* [(len    (string-length path-part-string))
                        (start  (substring path-part-string 0 1))
                        (middle (substring path-part-string 1 (sub1 len)))
                        (end    (substring path-part-string (sub1 len) len))
                        (pieces (string-split middle ":"))]
                   (and (string=? start "<")
                        (string=? end ">")
                        (= (length pieces) 2)
                        (> (string-length (second pieces)) 0)))))

;; String -> Boolean
;; Determine if the given PathPart string is in a valid PathParam form with a valid type
(define (valid-path-param? path-part-string)
  (and (path-param-shape? path-part-string)
       (valid-route-param-type?
        (first (string-split (substring path-part-string
                                        1
                                        (sub1 (string-length path-part-string)))
                             ":")))))

;; String -> Boolean
;; determine if a string is a valid representation of a route param type
(define (valid-route-param-type? s)
  (member? s VALID-ROUTE-PARAM-TYPES))

;; String -> PathParam
;; Convert a string that passes path-param-string? into a PathParam
(define (string->path-param path-part-string)
  (let [(parsed-path-param-pieces (string-split (substring path-part-string
                                                           1
                                                           (sub1 (string-length path-part-string)))
                                                ":"))]
    (ws-route-param (second parsed-path-param-pieces)
                    (string->symbol (first parsed-path-param-pieces)))))


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

;; RequestPath [List-of Route] -> [Maybe Route]
;; find the best matching route for the given RequestPath
(define (best-matching-route req-path routes)
  (let* [(same-length-routes    (filter
                                 (λ (r) (= (length req-path) (length (ws-route-path-temp r))))
                                 routes))
         (matched-results       (map
                                 (λ (r) (make-matched-route req-path r))
                                 same-length-routes))
         (basic-matching-routes (filter
                                 (λ (mr) (not (member? #false (second mr))))
                                 matched-results))]
    (select-preferred-route basic-matching-routes)))

;; [List-of MatchedRoute] -> [Maybe Route]
;; Using the routing rules and given routes that basic match to their match sets, select the
;; preferred route
(define (select-preferred-route mrs)
  (cond
    [(empty? mrs) #false] ; No matches
    [(empty? (rest mrs)) (first (first mrs))] ; One match!
    [(empty? (second (first mrs))) #false] ; Path part result is empty, multiple matches
    [else
     (let* [(exact-first-matches  (filter
                                   (λ (mr)
                                     (symbol=? 'exact (first (second mr))))
                                   mrs))
            (int-first-matches    (filter
                                   (λ (mr)
                                     (and (symbol=? 'param (first (second mr)))
                                          (symbol=? 'int
                                                    (ws-route-param-type
                                                     (first (ws-route-path-temp (first mr)))))))
                                   mrs))
            (string-first-matches (filter
                                   (λ (mr)
                                     (and (symbol=? 'param (first (second mr)))
                                          (symbol=? 'string
                                                    (ws-route-param-type
                                                     (first (ws-route-path-temp (first mr)))))))
                                   mrs))]
       (cond
         [(cons? exact-first-matches)
          (select-preferred-route (map trim-first-path-parts exact-first-matches))]
         [(cons? int-first-matches)
          (select-preferred-route (map trim-first-path-parts int-first-matches))]
         [(cons? string-first-matches)
          (select-preferred-route (map trim-first-path-parts string-first-matches))]
         [else #false]))]))

;; MatchedRoute -> MatchedRoute
;; Trim both the path-temp and the PathPartMatchResult by one item
(define (trim-first-path-parts mr)
  (list (ws-route (rest (ws-route-path-temp (first mr))) (ws-route-handler (first mr)))
        (rest (second mr))))


;; RequestPath Route -> MatchedRoute
;; compare each path part to the path-temp and return a list of the results in order along with the
;; original route in a list
(define (make-matched-route req-path route)
  (list route (map req-part-match-route-temp-part-res req-path (ws-route-path-temp route))))

;; ReqestPathPart PathPart -> PathPartMatchResult
;; Compare two path parts (request and template) and return a result to tell
;; if it was exact, a parameter, or did not match
(define (req-part-match-route-temp-part-res req-part temp-part)
  (cond
    [(string? temp-part)
     (if-else-false (string=? (ws-req-path-part-string req-part) temp-part)
                    'exact)]
    [(ws-route-param? temp-part)
     (if-else-false (member? (ws-route-param-type temp-part) (ws-req-path-part-types req-part))
                    'param)]))

;; String Route -> [List-of RouteArgs]
;; parse the given request path string according to the route
;; TODO
(define (parse-path-args req-path route)
  '())



(define-syntax if-else-false
  (syntax-parser
    [(_ cond do) #'(if cond do #false)]))

(define (member? x xs)
  (not (false? (member x xs))))

