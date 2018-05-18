#lang racket/base
(require racket/class
         racket/list
         racket/match
         data/interval-map
         "conversion.rkt"
         "lsp.rkt"
         "jsonrpc.rkt"
         "../lang/lexer.rkt")

(define (report uri doc-text doc-tokens doc-trace)
  (racket/colorize uri doc-text doc-tokens doc-trace)
  (text-document/publish-diagnostics uri doc-text doc-tokens doc-trace))

(define (change uri doc-text doc-tokens [doc-trace #f])
  (racket/colorize uri doc-text doc-tokens doc-trace))

;; Publish diagnostics notification
(define (text-document/publish-diagnostics uri doc-text doc-tokens doc-trace)
  (define diagnostics (flatten (map (exception->Diagnostics doc-text)
                                    (send doc-trace get-diagnostics))))
  (send-notification
   "textDocument/publishDiagnostics"
   (PublishDiagnosticsParams #:uri uri
                             #:diagnostics diagnostics)))

;; Racket colorize notification
(define (racket/colorize uri doc-text doc-tokens [doc-trace #f])
  (define semantic-colors (if doc-trace
                              (send doc-trace get-semantic-coloring)
                              (make-interval-map)))
  (define text (send doc-text get-text))
  (define next-token
    ((compose sexp-comment-reclassifier
              (semantic-reclassifier semantic-colors)
              skip-white
              list->producer)
      doc-tokens))
  (define tokens
    (for/list ([token (in-producer next-token eof-object?)])
      (match-define (list text type paren? start end mode) token)
      (hasheq 'kind (symbol->string type)
              'mode (symbol->string mode)
              'range (pos/pos->Range doc-text (sub1 start) (sub1 end)))))

  (send-notification "racket/colorize"
                     (hasheq 'uri uri
                             'tokens tokens)))

(provide report change)
