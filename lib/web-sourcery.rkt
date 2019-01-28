#lang racket

(provide )

(require web-server/servlet
         web-server/servlet-env)
 
(define (my-app req)
  (response/full 200
                 #"Ok"
                 (current-seconds)
                 TEXT/HTML-MIME-TYPE
                 '()
                 (list (string->bytes/utf-8
                        (first (map path/param-path
                                    (url-path (request-uri req))))))))
 
(serve/servlet my-app
               #:port 80
               #:command-line? #t
               #:servlet-regexp #rx""
               #:listen-ip #f
               #:stateless? #t)
