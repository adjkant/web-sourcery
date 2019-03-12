#lang scribble/manual

@;{------------------------------------------------------------------------------------------------}
@;{Requirements} 

@(require scribble/example
          racket/sandbox
          sql-sourcery
          (for-label "../main.rkt"
                     racket))

@(define sourcery-eval
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string]
                  [sandbox-memory-limit 50])
     (sandbox-path-permissions (list (list 'write "../")))
     (define me (make-evaluator 'racket))
     #;(me '(require "../main.rkt"))
     me))

@examples[#:eval sourcery-eval #:hidden]


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

@;{------------------------------------------------------------------------------------------------}
@section{Defining Routes and Handlers}

@defform[(define-route [app-name route [method ...]] -> return-type
           route-body)
         #:contracts([app-name id?]
                     [route string?]
                     [method http-method?]
                     [return-type (or JSON TEXT)]
                     [route-body any])
         ]{
 Defines a new route handler for the given app that matches on the given route.
 Things available in the body needing further documentation: method, params, cookies, headers
}

@defproc[(response [x (or string? json-serializable?)] [status http-status?])
         response?]{
 A constructor for a response for any route handler
}

@;{------------------------------------------------------------------------------------------------}
@section{HTTP Methods and Status Codes}

@subsection{HTTP Methods}

@defthing[GET http-method?]{
  HTTP GET
}

@defproc[(GET? [x any?]) boolean?]{
 A predicate for testing HTTP Methods                
}

@defthing[POST http-method?]{
  HTTP POST
}

@defproc[(POST? [x any?]) boolean?]{
 A predicate for testing HTTP Methods                
}

@defthing[PUT http-method?]{
  HTTP PUT
}

@defproc[(PUT? [x any?]) boolean?]{
 A predicate for testing HTTP Methods                
}

@defthing[DELETE http-method?]{
  HTTP DELETE
}

@defproc[(DELETE? [x any?]) boolean?]{
 A predicate for testing HTTP Methods                
}

@defproc[(method->symbol [m http-method?]) symbol?]{
 Translate an HTTP Method to a symbol           
}

@subsection{HTTP Statuses}

@defthing[200-OK http-status?]{
  HTTP Ok Response
}

@defthing[404-NOT-FOUND http-status?]{
  HTTP Not Found Response
}

@defproc[(custom-status [code natural?] [description string?]) http-status?]{
 A constructor for a custom HTTP statuses             
}


@;{------------------------------------------------------------------------------------------------}
@section{Working With JSON}

@subsection{JSON Object Shortcuts}

@defproc[(json-obj [kv-pairs (listof json-kv)])
         hash?]{
 A shortcut for creating JSON serializable objects with the given list of json-kv's
}

@defproc[(json-kv [key string?] [value json-serializable?])
         list?]{
 A semantic shortcut for creating JSON serializable pair in a list for a JSON object.
 Equivalent to list.
}

@subsection{JSON Serialization}

@defproc[(json-serializer-struct [struct-name struct?])
         json-serializer?]{
 Create a JSON serializer for the given struct using all fields of the structure
}

@defform[(json-serializer predicate (name accessor) ...)
         #:contracts([predicate struct?]
                     [name id]
                     [accessor (-> any? json-serializable?)])]{
 Create a JSON serializer for anything that passes the given predicate
 using the given names and accessor
}