#lang racket

(provide serve-from-folder)

(require racket/runtime-path
         "../data/conversion.rkt"
         "../data/defs.rkt")

(require (for-syntax syntax/parse
                     racket/syntax))

;; TODO check path exists
;; Add a static folder route to the given app
(define-syntax serve-from-folder
  (syntax-parser
    [(_ [app-name:id app-path:string] serve-path:string)
    #'(set! app-name
            (ws-app (cons (strings->static-folder-route
                           app-path
                           (string-append (path->string (current-directory)) serve-path))
                          (ws-app-static-routes app-name))
                    (ws-app-routes app-name)))]))