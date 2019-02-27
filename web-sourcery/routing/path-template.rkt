#lang racket

(provide string->path-template
         analagous-path-template?
         get-param-names)

(require "../data-defs.rkt"
         "../utils/basics.rkt")

(module+ test (require "../utils/testing.rkt"))

(define MIN-PATH-PARAM-STRING-SIZE (string-length "<int:a>"))
(define VALID-ROUTE-PARAM-TYPES (list "string" "int"))

;; PathTemplate PathTemplate -> Boolean
;; determine if the two given path templates would match to the same route sets
(define (analagous-path-template? t1 t2)
  #t)

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

(module+ test
  (check-equal? (string->path-template "/hello/world/") PATH-TEMP-1)
  (check-equal? (string->path-template "/hello/world") PATH-TEMP-1)
  (check-equal? (string->path-template "hello/world") PATH-TEMP-1)
  (check-equal? (string->path-template "/<string:matched-string>") PATH-TEMP-3)
  (check-equal? (string->path-template "/<int:matched-int>") PATH-TEMP-4)
  (check-equal? (string->path-template "/<int:i1>/<int:i2>/<int:i3>") PATH-TEMP-6)
  (check-false (string->path-template "/<s:matched-string>")))

;; String -> Boolean
;; determine if the string is a valid PathPart
(define (valid-path-part? path-part)
  (or (valid-path-param? path-part)
      (not (path-param-shape? path-part))))

(module+ test
  (check-true (valid-path-part? "<int:i>"))
  (check-true (valid-path-part? "<string:s>"))
  (check-true (valid-path-part? "i"))
  (check-false (valid-path-part? "<i:int>"))
  (check-false (valid-path-part? "<int:>"))
  (check-false (valid-path-part? "<:i>")))

;; String -> Boolean
;; Determine if the given PathPart string should be checked for valid PathParam form
(define (path-param-shape? path-part-string)
  (let* [(len    (string-length path-part-string))
         (start  (substring path-part-string 0 1))
         (end    (substring path-part-string (sub1 len) len))]
    (and (string=? start "<")
         (string=? end ">"))))

(module+ test
  (check-true (path-param-shape? "<int:i>"))
  (check-true (path-param-shape? "<string:s>"))
  (check-false (path-param-shape? "i"))
  (check-true (path-param-shape? "<i:int>"))
  (check-true (path-param-shape? "<int:>"))
  (check-true (path-param-shape? "<:i>")))

;; String -> Boolean
;; Determine if the given PathPart string is in a valid PathParam form
(define (exact-path-param-shape? path-part-string)
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

(module+ test
  (check-true (exact-path-param-shape? "<int:i>"))
  (check-true (exact-path-param-shape? "<string:s>"))
  (check-false (exact-path-param-shape? "i"))
  (check-true (exact-path-param-shape? "<i:int>"))
  (check-false (exact-path-param-shape? "<int:>"))
  (check-false (exact-path-param-shape? "<:i>")))

;; String -> Boolean
;; Determine if the given PathPart string is in a valid PathParam form with a valid type
(define (valid-path-param? path-part-string)
  (and (exact-path-param-shape? path-part-string)
       (valid-route-param-type?
        (first (string-split (substring path-part-string
                                        1
                                        (sub1 (string-length path-part-string)))
                             ":")))))

(module+ test
  (check-true (valid-path-param? "<int:i>"))
  (check-true (valid-path-param? "<string:s>"))
  (check-false (valid-path-param? "i"))
  (check-false (valid-path-param? "<i:int>"))
  (check-false (valid-path-param? "<int:>"))
  (check-false (valid-path-param? "<:i>")))

;; String -> Boolean
;; determine if a string is a valid representation of a route param type
(define (valid-route-param-type? s)
  (member? s VALID-ROUTE-PARAM-TYPES))

(module+ test
  (check-true (valid-route-param-type? "int"))
  (check-true (valid-route-param-type? "string"))
  (check-false (valid-route-param-type? "i"))
  (check-false (valid-route-param-type? "s"))
  (check-false (valid-route-param-type? "str"))
  (check-false (valid-route-param-type? "racket")))

;; String -> PathParam
;; Convert a string that passes path-param-string? into a PathParam
(define (string->path-param path-part-string)
  (let [(parsed-path-param-pieces (string-split (substring path-part-string
                                                           1
                                                           (sub1 (string-length path-part-string)))
                                                ":"))]
    (ws-route-param (second parsed-path-param-pieces)
                    (string->symbol (first parsed-path-param-pieces)))))

(module+ test
  (check-equal? (string->path-param "<string:matched-string>") ROUTE-PARAM-STR)
  (check-equal? (string->path-param "<int:matched-int>") ROUTE-PARAM-INT)
  (check-equal? (string->path-param "<int:i1>") ROUTE-PARAM-I1))


;; PathTemplate -> [List-of String]
(define (get-param-names path-template)
  (map ws-route-param-name (filter ws-route-param? path-template)))

(module+ test
  (check-equal? (get-param-names PATH-TEMP-1) '())
  (check-equal? (get-param-names PATH-TEMP-3) '("matched-string"))
  (check-equal? (get-param-names PATH-TEMP-4) '("matched-int"))
  (check-equal? (get-param-names PATH-TEMP-6) '("i1" "i2" "i3")))