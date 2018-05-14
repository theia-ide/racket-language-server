#lang racket/base
(require racket/class
         "lsp.rkt")

(define (pos->line/char t pos)
  (define line (send t position-paragraph pos))
  (define line-begin (send t paragraph-start-position line))
  (define char (- pos line-begin))
  (values line char))

(define (line/char->pos t line char)
  (+ char (send t paragraph-start-position line)))

(define (pos->Position t pos)
  (define-values (line char) (pos->line/char t pos))
  (make-Position line char))

(define (pos/pos->Range t start end)
  (Range #:start (pos->Position t start)
         #:end (pos->Position t end)))

(provide (all-defined-out))
