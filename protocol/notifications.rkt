#lang racket/base
(require racket/class
         racket/list
         racket/match
         "conversion.rkt"
         "lsp.rkt"
         "jsonrpc.rkt"
         "../lang/lexer.rkt")

(define (report uri text trace)
  (racket/colorize-semantic uri text trace)
  (text-document/publish-diagnostics uri text trace))

(define (change uri text)
  (racket/colorize uri text))

;; Publish diagnostics notification
(define (text-document/publish-diagnostics uri doc-text doc-trace)
  (define diagnostics (flatten (map (exception->Diagnostics doc-text)
                                    (send doc-trace get-diagnostics))))
  (send-notification
   "textDocument/publishDiagnostics"
   (PublishDiagnosticsParams #:uri uri
                             #:diagnostics diagnostics)))

;; Racket colorize notification
(define (racket/colorize uri doc-text)
  (define text (send doc-text get-text))
  (define next-token
    (lang-tokenizer
     (sexp-comment-reclassifier
      (skip-white
       (make-tokenizer text)))))
  (define tokens
    (for/list ([token (in-producer next-token eof-object?)])
      (match-define (list text type paren? start end mode) token)
      (hasheq 'kind (symbol->string type)
              'mode (symbol->string mode)
              'range (pos/pos->Range doc-text (sub1 start) (sub1 end)))))

  (send-notification "racket/colorize"
                     (hasheq 'uri uri
                             'tokens tokens)))

;; Racket semantic coloring notification
(define (racket/colorize-semantic uri doc-text doc-trace)
  (define colors (send doc-trace get-semantic-coloring))
  (define tokens
    (map (match-lambda
           [(list start end type)
            (hasheq 'kind type
                    'range (pos/pos->Range doc-text (sub1 start) (sub1 end)))])
         colors))
  (send-notification "racket/colorize-semantic"
                     (hasheq 'uri uri
                             'tokens tokens)))

(provide report change)
