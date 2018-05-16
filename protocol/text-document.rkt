#lang racket/base
(require racket/contract/base
         racket/class
         racket/match
         data/interval-map
         "conversion.rkt"
         "lsp.rkt"
         "json-util.rkt")

;; Hover request
(define-json-expander TextDocumentPositionParams
  [textDocument any/c]
  [position any/c])

(define-json-expander Hover
  [contents string?]
  [range any/c])

(define (text-document/hover ws params)
  (match-define
    (TextDocumentPositionParams
     #:textDocument (TextDocumentIdentifier #:uri uri)
     #:position (Position #:line line #:character char))
    params)
  (define-values (doc-text doc-trace) (send ws open-doc uri))
  (define hovers (send doc-trace get-hovers))
  (define pos (line/char->pos doc-text line char))
  (define-values (start end text)
    (interval-map-ref/bounds hovers pos #f))

  (if text
      (Hover #:contents text
             #:range (pos/pos->Range doc-text start end))
      'null))

(provide text-document/hover)
