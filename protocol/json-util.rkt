#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/parse/experimental/template)
         racket/match
         syntax/parse)

(define-syntax (define-json-expander stx)
  (syntax-parse stx
    [(_ name:id [key:id ctc:expr] ...+)
     (with-syntax ([(key_ ...) (generate-temporaries #'(key ...))]
                   [(keyword ...)
                    (for/list ([k (syntax->datum #'(key ...))])
                      (string->keyword (symbol->string k)))])
       (syntax/loc stx
         (define-match-expander name
           (λ (stx)
             (syntax-parse stx
               [(_ (~optional (~seq keyword key_)) ...)
                (quasitemplate/loc stx (hash-table (?? ['key (? ctc key_)]) ...))]))
           (λ (stx)
             (syntax-parse stx
               [(_ (~optional (~seq keyword key_)) ...)
                (syntax/loc stx
                  (make-hasheq (list (cons 'key key_) ...)))])))))]))

(module+ test
  (require rackunit)

  (define-json-expander greeting
    [hello string?]
    [who string?])

  (define hello_world (greeting #:hello "hello" #:who "world"))
  (check-equal? (match hello_world
                  [(greeting #:hello hello) hello])
                "hello")
  (check-equal? (match hello_world
                  [(greeting #:who who) who])
                "world")

  ;; No support for constructing partial elements
  #;(define hello (greeting #:hello "hello"))
  #;(check-equal? (match hello
                  [(greeting #:hello hello) hello])
                "hello")
  )

(provide define-json-expander)
