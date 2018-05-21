#lang racket/base
(require racket/class
         racket/list
         racket/match
         "check-syntax.rkt"
         "lexer.rkt")

(struct document (uri version text))
(struct tokenized-document (uri version text tokens))
(struct traced-document (uri version text tokens trace))

(define (make-tokenized-document doc)
  (match-define (document uri version text) doc)

  (define tokens
    (apply-tokenizer-maker
     (compose lang-tokenizer make-tokenizer)
     text))

  (tokenized-document uri version text tokens))

(define (uri->path uri)
  (substring uri 7))

(define (make-traced-document doc)
  (match-define (tokenized-document uri version text tokens) doc)

  (define trace
    (check-syntax (uri->path uri) text))

  (traced-document uri version text tokens trace))

(define (document:uri doc)
  (match doc
    [(document uri _ _) uri]
    [(tokenized-document uri _ _ _) uri]
    [(traced-document uri _ _ _ _) uri]))

(define (document:version doc)
  (match doc
    [(document _ version _) version]
    [(tokenized-document _ version _ _) version]
    [(traced-document _ version _ _ _) version]))

(define (document:text doc)
  (match doc
    [(document _ _ text) text]
    [(tokenized-document _ _ text _) text]
    [(traced-document _ _ text _ _) text]))

(define (document:tokens doc)
  (match doc
    [(document _ _ _) #f]
    [(tokenized-document _ _ _ tokens) tokens]
    [(traced-document _ _ _ tokens _) tokens]))

(define (document:trace doc)
  (match doc
    [(document _ _ _) #f]
    [(tokenized-document _ _ _ _) #f]
    [(traced-document _ _ _ _ trace) trace]))

(define (has-syntax-error doc)
  (match-define (traced-document _ _ _ _ trace) doc)
  (not (empty? (send trace get-errors))))

(provide (all-defined-out))
