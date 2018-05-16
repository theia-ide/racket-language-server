#lang racket/base
(require racket/class
         racket/match
         data/interval-map
         "conversion.rkt"
         "jsonrpc.rkt" ; send-notification
         "lsp.rkt"
         "../lang/lexer.rkt")

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

;; Synchronization methods
(define (text-document/did-open ws params)
  (match-define
    (DidOpenTextDocumentParams
     #:textDocument (TextDocumentItem #:uri uri
                                      #:text text))
    params)
  (send ws add-doc uri text)

  ;; Send colorize notification
  (racket/colorize ws uri))

(define (text-document/did-close ws params)
  (match-define
    (DidCloseTextDocumentParams
     #:textDocument (TextDocumentIdentifier #:uri uri))
    params)
  (send ws remove-doc uri))

(define (text-document/did-change ws params)
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

;; Text document methods
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

;; Racket methods

;; Racket colorize notification
;;
;; Called when a text document open or text document change event is received
(define (racket/colorize ws uri)
  (define doc-text (send ws get-doc-text uri))
  (define text (send ws open-doc-text uri))

  (define next-token (make-tokenizer text))
  (define tokens
    (for/list ([token (in-producer next-token eof-object?)])
      (match-define (list text type paren? start end) token)
      (hasheq 'kind (symbol->string type)
              'range (pos/pos->Range doc-text (sub1 start) (sub1 end)))))
  (send-racket/colorize uri tokens))

(define (send-racket/colorize uri tokens)
  (send-notification "racket/colorize"
                     (hasheq 'uri uri
                             'tokens tokens)))

(provide (all-defined-out))
