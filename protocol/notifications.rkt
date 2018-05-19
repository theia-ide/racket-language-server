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
  (when (empty? (send doc-trace get-errors))
    ;; Skip semantic coloring when there are errors in the trace
    ;; since that will remove the existing semantic coloring if
    ;; the previous syntax check was successful.
    (racket/colorize uri doc-text doc-tokens doc-trace))
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
  (define text (send doc-text get-text))
  (define semantic-colors (if doc-trace
                              (send doc-trace get-semantic-coloring)
                              (make-interval-map)))
  (define errors (if doc-trace
                     (send doc-trace get-errors)
                     '()))
  (define next-token
    (sexp-comment-reclassifier
     (skip-white
      ((semantic-reclassifier semantic-colors errors)
       (list->producer doc-tokens)))))

  (define tokens
    (for/list ([tok (in-producer next-token void?)])
      (match-define (token text type data start end mode diff) tok)
      (hasheq 'kind (symbol->string (if (and (eq? type 'symbol) data) data type))
              'mode (symbol->string mode)
              'range (pos/pos->Range doc-text (sub1 start) (sub1 end)))))

  (send-notification "racket/colorize"
                     (hasheq 'uri uri
                             'tokens tokens)))

(provide report change)
