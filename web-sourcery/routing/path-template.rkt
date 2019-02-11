#lang racket

(provide string->path-template
         get-param-names)

(require "ws-route.rkt"
         "../utils.rkt")

(define MIN-PATH-PARAM-STRING-SIZE (string-length "<int:a>"))
(define VALID-ROUTE-PARAM-TYPES (list "string" "int"))

;; String -> [Maybe PathTemplate]
;; convert a string into a PathTemplate or #false if any part is invalid
(define (string->path-template path)
  (let* [(split-path (string-split path "/"))]
    (when/f (andmap valid-path-part? split-path)
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
  (when/f (>= (string-length path-part-string) MIN-PATH-PARAM-STRING-SIZE)
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


;; PathTemplate -> [List-of String]
(define (get-param-names path-template)
  (map ws-route-param-name (filter ws-route-param? path-template)))