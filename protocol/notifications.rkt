#lang racket/base
(require racket/class
         racket/list
         racket/match
         "conversion.rkt"
         "lsp.rkt"
         "jsonrpc.rkt"
         "../lang/lexer.rkt")

(define (report uri doc-text doc-tokens doc-trace)
  (racket/colorize-semantic uri doc-text doc-tokens doc-trace)
  (text-document/publish-diagnostics uri doc-text doc-tokens doc-trace))

(define (change uri doc-text doc-tokens)
  (racket/colorize uri doc-text doc-tokens))

;; Publish diagnostics notification
(define (text-document/publish-diagnostics uri doc-text doc-tokens doc-trace)
  (define diagnostics (flatten (map (exception->Diagnostics doc-text)
                                    (send doc-trace get-diagnostics))))
  (send-notification
   "textDocument/publishDiagnostics"
   (PublishDiagnosticsParams #:uri uri
                             #:diagnostics diagnostics)))

;; Racket colorize notification
(define (racket/colorize uri doc-text doc-tokens)
  (define text (send doc-text get-text))
  (define next-token
    (sexp-comment-reclassifier
     (skip-white
      (list->producer doc-tokens))))
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
(define (racket/colorize-semantic uri doc-text doc-tokens doc-trace)
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
