#lang racket/base
(require racket/class
         racket/list
         racket/match
         data/interval-map
         "conversion.rkt"
         "lsp.rkt"
         "jsonrpc.rkt"
         "../lang/document.rkt"
         "../lang/lexer.rkt")

(define (on-trace doc)
  (define doc-text (document->text% doc))
  (racket/colorize doc doc-text)
  (text-document/publish-diagnostics doc doc-text))

(define (on-tokenize doc)
  (define doc-text (document->text% doc))
  (racket/colorize doc doc-text))

;; Publish diagnostics notification
(define (text-document/publish-diagnostics doc doc-text)
  (match-define (traced-document uri _ text _ trace) doc)

  (define diagnostics (flatten (map (exception->Diagnostics doc-text)
                                    (send trace get-diagnostics))))
  (send-notification
   "textDocument/publishDiagnostics"
   (PublishDiagnosticsParams #:uri uri
                             #:diagnostics diagnostics)))

;; Racket colorize notification
(define last-traced-document #f)
(define (racket/colorize doc doc-text)
  (define new-trace (document:trace doc))
  (when (and new-trace (not (has-syntax-error doc)))
      (set! last-traced-document doc))

  (define next-token
    (if last-traced-document
        ((compose
         sexp-comment-reclassifier
         skip-white
         (semantic-reclassifier
          (send (document:trace last-traced-document) get-semantic-coloring)
          #t)
         (token-stream-matcher (document:tokens last-traced-document)))
         (list->producer (document:tokens doc)))
        ((compose
         sexp-comment-reclassifier
         skip-white)
         (list->producer (document:tokens doc)))))

  (define tokens
    (for/list ([tok (in-producer next-token eof-token?)])
      (match-define (token text type data start end mode diff) tok)
      (hasheq 'kind (symbol->string (if (and (eq? type 'symbol) data) data type))
              'mode (symbol->string mode)
              'range (pos/pos->Range doc-text (sub1 start) (sub1 end)))))

  (send-notification "racket/colorize"
                     (hasheq 'uri (document:uri doc)
                             'tokens tokens)))

(provide on-tokenize on-trace)
