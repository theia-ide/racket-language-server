#lang racket/base
(require racket/class
         racket/match
         racket/list
         data/interval-map
         "conversion.rkt"
         "lsp.rkt"
         "../lang/document.rkt")

;; Mutable variables
(define already-initialized? #f)
(define already-shutdown? #f)

;; Lifecycle methods
(define (lsp/initialize ws params)
  (match-define
    (InitializeParams #:processId processId
                      #:capabilities capabilities)
    params)
  (set! already-initialized? #t)
  (InitializeResult
   #:capabilities
   (hasheq 'textDocumentSync
           (TextDocumentSyncOptions
            #:openClose #t
            #:change TextDocumentSyncKindIncremental
            #:willSave #f
            #:willSaveWaitUntil #f
            #:save #f)
           'hoverProvider #t)))

(define (lsp/initialized ws params) #f)

(define (lsp/shutdown ws params)
  (set! already-shutdown? #t)
  'null)

(define (lsp/exit ws params)
  (exit (if already-shutdown? 0 1)))

(define (lsp/cancel-request ws params) #f)

;; Synchronization methods
(define (text-document/did-open ws params)
  (match-define
    (DidOpenTextDocumentParams
     #:textDocument (TextDocumentItem #:uri uri
                                      #:text text))
    params)
  (send ws notify-open uri text))

(define (text-document/did-close ws params)
  (match-define
    (DidCloseTextDocumentParams
     #:textDocument (TextDocumentIdentifier #:uri uri))
    params)
  (send ws notify-close uri))

(define (text-document/did-change ws params)
  (match-define
    (DidChangeTextDocumentParams
     #:textDocument (VersionedTextDocumentIdentifier #:uri uri)
     #:contentChanges content-changes)
    params)

  (define doc (send ws request uri))
  (define doc-text (document->text% doc))
  (for ([change (in-list content-changes)])
    (match change
      [(TextDocumentContentChangeEvent
        #:range (Range #:start (Position #:line st-ln #:character st-ch))
        #:rangeLength range-ln
        #:text text)
       (define st-pos (line/char->pos doc-text st-ln st-ch))
       (define end-pos (+ st-pos range-ln))
       (send ws notify-update uri text st-pos end-pos)]
      [(TextDocumentContentChangeEvent #:text text)
       (send ws notify-replace uri text)])))

;; Text document methods
(define (text-document/hover ws params)
  (match-define
    (TextDocumentPositionParams
     #:textDocument (TextDocumentIdentifier #:uri uri)
     #:position (Position #:line line #:character char))
    params)
  (define doc (send ws request-traced uri))
  (define doc-text (document->text% doc))
  (define hovers (send (document:trace doc) get-hovers))
  (define pos (line/char->pos doc-text line char))
  (define-values (start end text)
    (interval-map-ref/bounds hovers pos #f))

  (if text
      (Hover #:contents text
             #:range (pos/pos->Range doc-text start end))
      'null))

(provide (all-defined-out))
