#lang racket

;; TODO rename to ws-structs

(provide (struct-out ws-app)
         (struct-out ws-static-folder-route)
         (struct-out ws-route)
         (struct-out ws-method)
         (struct-out ws-route-param)
         (struct-out ws-req-path-part)
         (struct-out ws-matched-route)
         (struct-out ws-request)
         (struct-out ws-response)
         VALID-RESPONSE-TYPES
         (struct-out ws-status)
         (struct-out ws-query-param)
         (struct-out ws-header)
         (struct-out ws-cookie)
         (struct-out ws-file))


(struct ws-app [static-routes routes] #:transparent)
;; A WSApp is a (ws-app [List-of StaticRoute] [List-of Route])

;; A StaticRoute is one of:
;; - StaticFolderRoute

(struct ws-static-folder-route [app-path file-path])
;; A StaticFolderRoute is a (ws-static-folder-route [List-of String] [List-of String])

;; A Route (ws-route PathTemplate [List-of Method] ResponseType RouteHandler)
(struct ws-route [path-temp methods response-type handler] #:transparent)

;; A RouteHandler is a:
;; [RouteArg ... [String -> [Maybe String]] [String -> [Maybe String]] [String -> [Maybe String]]
;;  ->
;;  Response]

;; PathTemplate is a [List-of PathPart]

;; PathPart is one of:
;; - String
;; - PathParam

(struct ws-route-param [name type] #:transparent)
;; A PathParam is a (ws-route-param String PathParamType)

;; A PathParamType is one of:
;; - 'string
;; - 'int

;; A ResponseType is one of:
;; - 'TEXT
;; - 'JSON

(define VALID-RESPONSE-TYPES '(TEXT JSON))

(struct ws-method [m] #:transparent)
;; A Method is one of:
;; - (ws-method 'GET)
;; - (ws-method 'POST)
;; - (ws-method 'UPDATE)
;; - (ws-method 'DELETE)

;; MatchedRoute is a (ws-matched-route Route PathTemplate [List-of PathPartMatchResult])
(struct ws-matched-route [full parts results] #:transparent)

;; PathPartMatchResult is one of:
;; - 'exact
;; - 'param
;; - #false

;; A JSONSource is one of:
;; - 'json
;; - 'form
;; - 'url
;; - 'none

(struct ws-file [field filename content-type contents] #:transparent)
;; A File is a (ws-file String String String Bytes)

(struct ws-request [method path query-params headers cookies json json-source files]
  #:transparent)

;; A Request is a
;; (ws-request Method RequestPath
;;             [List-of QueryParam] [List-of Header] [List-of Cookie]
;;             [Maybe jsexpr] JSONSource [List-of File])

;; RequestPath is a [List-of RequestPathPart]

(struct ws-req-path-part [string types] #:transparent)
;; RequestPathPart is a (ws-req-path-part String [List-of PathParamType])

;; A RouteArg is one of:
;; - Integer
;; - String

(struct ws-status [code description] #:transparent)
;; A StatusCode is a (ws-status-code Number String)
;; where the number is a valid HTTP status code as defined at the following link:
;; https://www.restapitutorial.com/httpstatuscodes.html

(struct ws-query-param [name value] #:transparent)
;; A QueryParam is a (ws-query-param String [Maybe String])

(struct ws-header [field value] #:transparent)
;; A Header is a (ws-header String String)

(struct ws-cookie [name value] #:transparent)
;; A Cookie is a (ws-header String String)

;; A Response is one of:
;; - DataResponse
;; - FileResponse
;; - ResponseErrorCode

(struct ws-response [data status headers cookies] #:transparent)
;; A DataResponse is a (ws-response Any StatusCode [List-of Header] [List-of Cookie])

;; A FileResponse is a String
;; interpretation: the full path to the given file to respond with

;; A ResponseErrorCode is one of:
;; - 404
;; - 500
