#lang scribble/manual

@;{------------------------------------------------------------------------------------------------}
@;{Requirements} 

@(require scribble/example
          racket/sandbox
          sql-sourcery
          "../main.rkt"
          (for-label "../main.rkt"))

@(define sourcery-eval
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string]
                  [sandbox-memory-limit 50])
     (sandbox-path-permissions (list (list 'write "../")))
     (make-evaluator 'racket
                     #:requires (list "../main.rkt"))))

@examples[#:eval sourcery-eval
          #:hidden]


@;{------------------------------------------------------------------------------------------------}
@;{Documentation Start} 

@title{WebSourcery}

@author{Adrian Kant}

A Web Microframework for Racket

In Development

@(hyperlink "https://github.com/adjkant/web-sourcery"
             "Github Repo")

@defmodule[web-sourcery]

@;{------------------------------------------------------------------------------------------------}
@section{Creating and Running WebSourcery App}

@defform[(define-web-sourcery-app app-name)
         #:contracts([app-name id?])
         ]{
 Define a new WebSourcery app to add routes to.
}

@defform[(run-web-sourcery-app app-name)
         #:contracts([app-name id?])
         ]{
 Run an exisiting WebSourcery app.
}

@defform[(define-route [app-name route]
           route-body)
         #:contracts([app-name id?]
                     [route string?]
                     [route-body any])
         ]{
 Defines a new route handler for the given app that matches on the given route.
}