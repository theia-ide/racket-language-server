#lang info
(define collection "racket-language-server")
(define version "0.0.1")
(define scribblings '(("scribblings/racket-language-server.scrbl")))
(define deps '("base"
               "data-lib"
               "drracket-tool-lib"
               "gui-lib"
               "syntax-color-lib"))
(define build-deps '("racket-doc"
                     "rackunit-lib"
                     "scribble-lib"))
