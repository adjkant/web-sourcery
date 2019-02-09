#lang racket

(provide define-web-sourcery-app
         define-route
         run-web-sourcery-app
         (all-from-out sql-sourcery))

(require web-server/servlet
         web-server/servlet-env
         sql-sourcery
         (for-syntax syntax/parse
                     racket/syntax))


;; Internal Data Definitions
(struct ws-route [path handler])

;; Constants
(define DEFAULT-PORT 9000)
(define DEFAULT-CORS? #false)
(define DEFAULT-PUBLIC? #false)


;; Top Level WebSourcery App Definition
(define-syntax define-web-sourcery-app
  (syntax-parser
    [(_ app-name:id) #'(define app-name '())]))


;; Top Level Route Definition
;; TODO give name for debugging
(define-syntax define-route
  (syntax-parser
    [(_ [app-name:id path:string] route-body)
     #'(set! app-name (cons (ws-route path (λ () route-body)) app-name))]))


;; Route a request to the apropriate handler
(define (handle-route req app)
  (let* [(path (first (map path/param-path (url-path (request-uri req)))))
         (matched-route (filter (λ (r) (string=? (substring (ws-route-path r) 1) path))
                                app))]
    (if (> (length matched-route) 0)
        (string->bytes/utf-8 ((ws-route-handler (first matched-route))))
        (string->bytes/utf-8 "no matching route"))))


;; Start a WebSourcery Server
;; TODO https://docs.racket-lang.org/syntax/wrapc.html?q=syntax%20contract
(define-syntax run-web-sourcery-app
  (syntax-parser
    [(_ app-name
        (~alt (~optional (~seq #:port port:integer)       #:defaults ([port #'DEFAULT-PORT]))
              (~optional (~seq #:cors? cors?:boolean)     #:defaults ([cors? #'DEFAULT-CORS?]))
              (~optional (~seq #:public? public?:boolean) #:defaults ([public? #'DEFAULT-PUBLIC?])))
        ...)
     #'(begin
         (define (app-main req)
           (response/full 200
                          #"Ok"
                          (current-seconds)
                          TEXT/HTML-MIME-TYPE
                          '()
                          (list (handle-route req app-name))))
         (serve/servlet app-main
                        #:port port
                        #:command-line? #t
                        #:servlet-regexp #rx""
                        #:listen-ip public?
                        #:stateless? #t))]))
