#lang racket/base
(require racket/class
         racket/contract/base
         racket/match
         "conversion.rkt"
         "json-util.rkt"
         "lsp.rkt"
         "racket.rkt")

;; DidOpenTextDocumentParams
(define-json-expander DidOpenTextDocumentParams
  [textDocument any/c])

(define (did-open ws params)
  (match-define
    (DidOpenTextDocumentParams
     #:textDocument (TextDocumentItem #:uri uri
                                      #:text text))
    params)
  (send ws add-doc uri text)

  ;; Send colorize notification
  (racket/colorize ws uri))

;; DidCloseTextDocumentParams
(define-json-expander DidCloseTextDocumentParams
  [textDocument any/c])

(define (did-close ws params)
  (match-define
    (DidCloseTextDocumentParams
     #:textDocument (TextDocumentIdentifier #:uri uri))
    params)
  (send ws remove-doc uri))

;; DidChangeTextDocumentParams
(define-json-expander DidChangeTextDocumentParams
  [textDocument any/c]
  [contentChanges list?])

(define-json-expander TextDocumentContentChangeEvent
  [range any/c]
  [rangeLength exact-nonnegative-integer?]
  [text string?])

(define (did-change ws params)
  (match-define
    (DidChangeTextDocumentParams
     #:textDocument (VersionedTextDocumentIdentifier #:uri uri)
     #:contentChanges content-changes)
    params)

  (define doc-text (send ws get-doc-text uri))
  (for ([change (in-list content-changes)])
    (match change
      [(TextDocumentContentChangeEvent
        #:range (Range #:start (Position #:line st-ln #:character st-ch))
        #:rangeLength range-ln
        #:text text)
       (define st-pos (line/char->pos doc-text st-ln st-ch))
       (define end-pos (+ st-pos range-ln))
       (send ws update-doc uri text st-pos end-pos)]
      [(TextDocumentContentChangeEvent #:text text)
       (send ws replace-doc uri text)]))

  ;; Send colorize notification
  (racket/colorize ws uri))

(provide (all-defined-out))
