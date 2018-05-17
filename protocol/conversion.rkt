#lang racket/base
(require racket/class
         racket/list
         racket/match
         "lsp.rkt"
         "../lang/check-syntax.rkt") ; For exception and warning structs

(define (pos->line/char t pos)
  (define line (send t position-paragraph pos))
  (define line-begin (send t paragraph-start-position line))
  (define char (- pos line-begin))
  (values line char))

(define (line/char->pos t line char)
  (+ char (send t paragraph-start-position line)))

(define (pos->Position t pos)
  (define-values (line char) (pos->line/char t pos))
  (Position #:line line #:character char))

(define (pos/pos->Range t start end)
  (Range #:start (pos->Position t start)
         #:end (pos->Position t end)))

(define (srcloc->Range sl)
  (match-define (srcloc src line col pos span) sl)
  (Range #:start (Position #:line (sub1 line) #:character col)
         #:end (Position #:line (sub1 line) #:character (+ col span))))

(define (exception->Diagnostics e)
  (define-values (code msg srclocs severity)
    (match e
      [(exception code msg srclocs)
       (values code msg srclocs DiagnosticSeverityError)]
      [(warning code msg srclocs)
       (values code msg srclocs DiagnosticSeverityWarning)]))
  (map (lambda (sl)
         (Diagnostic #:range (srcloc->Range sl)
                     #:message msg
                     #:severity severity
                     #:code code
                     #:source "Racket"
                     #:relatedInformation empty))
       srclocs))

(provide (all-defined-out))
