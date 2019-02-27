#lang racket

;; TODO rename to ws-structs

(provide (struct-out ws-route)
         (struct-out ws-method)
         (struct-out ws-route-param)
         (struct-out ws-req-path-part)
         (struct-out ws-matched-route)
         (struct-out ws-request)
         (struct-out ws-query-param)
         (struct-out ws-header)
         (struct-out ws-cookie))


;; A WSApp is a [List-of Route]

;; A Route (ws-route PathTemplate [List-of Method] RouteHandler)
(struct ws-route [path-temp methods handler] #:transparent)

;; A RouteHandler is a [RouteArg ... [String -> [Maybe Header]] [String -> [Maybe Cookie]] -> String]

;; PathTemplate is a [List-of PathPart]

;; PathPart is one of:
;; - String
;; - PathParam

(struct ws-route-param [name type] #:transparent)
;; A PathParam is a (ws-route-param String PathParamType)

;; A PathParamType is one of:
;; - 'string
;; - 'int

;; RequestPath is a [List-of RequestPathPart]

(struct ws-req-path-part [string types] #:transparent)
;; RequestPathPart is a (ws-req-path-part String [List-of PathParamType])

;; A RouteArg is one of:
;; - Integer
;; - String

;; MatchedRoute is a (ws-matched-route Route PathTemplate [List-of PathPartMatchResult])
(struct ws-matched-route [full parts results] #:transparent)

;; PathPartMatchResult is one of:
;; - 'exact
;; - 'param
;; - #false

(struct ws-request [method path query-params headers cookies] #:transparent)
;; A Request is a
;; (ws-request Method [List-of RequestPathPart] [List-of QueryParam [List-of Header] [List-of Cookie])

(struct ws-method [m] #:transparent)
;; A Method is one of:
;; - (ws-method 'GET)
;; - (ws-method 'POST)
;; - (ws-method 'UPDATE)
;; - (ws-method 'DELETE)

(struct ws-query-param [name value] #:transparent)
;; A QueryParam is a (ws-query-param String [Maybe String])

(struct ws-header [field value] #:transparent)
;; A Header is a (ws-header String String)

(struct ws-cookie [name value] #:transparent)
;; A Cookie is a (ws-header String String)
