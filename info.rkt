#lang info
(define collection "racket-language-server")
(define version "0.0.1")
(define scribblings '(("scribblings/racket-language-server.scrbl")))
(define deps '("base"
               "data-lib"
               "drracket-tool-lib"
               "gui-lib"
               "scribble-lib"
               "syntax-color-lib"))
(define build-deps '("at-exp-lib"
                     "data-doc"
                     "racket-doc"
                     "rackunit-lib"
                     "scribble-lib"))
