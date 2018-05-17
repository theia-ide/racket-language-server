#lang racket/base
(require racket/class
         racket/list
         racket/match
         "conversion.rkt"
         "lsp.rkt"
         "jsonrpc.rkt"
         "../lang/lexer.rkt")

(define (report uri trace)
  (text-document/publish-diagnostics uri trace))

(define (change uri text)
  (racket/colorize uri text))

;; Publish diagnostics notification
(define (text-document/publish-diagnostics uri doc-trace)
  (define diagnostics (flatten (map exception->Diagnostics
                                    (send doc-trace get-diagnostics))))
  (send-notification
   "textDocument/publishDiagnostics"
   (PublishDiagnosticsParams #:uri uri
                             #:diagnostics diagnostics)))

;; Racket colorize notification
(define (racket/colorize uri doc-text)
  (define text (send doc-text get-text))
  (define next-token (make-tokenizer text))
  (define tokens
    (for/list ([token (in-producer next-token eof-object?)])
      (match-define (list text type paren? start end) token)
      (hasheq 'kind (symbol->string type)
              'range (pos/pos->Range doc-text (sub1 start) (sub1 end)))))

  (send-notification "racket/colorize"
                     (hasheq 'uri uri
                             'tokens tokens)))

(provide report change)
