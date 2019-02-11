#lang racket

(provide (struct-out ws-route)
         (struct-out ws-route-param))

(struct ws-route [path-temp handler])
(struct ws-route-param [name type])

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