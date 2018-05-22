#lang racket/base
(require racket/class
         racket/match
         racket/list
         data/interval-map
         "conversion.rkt"
         "lsp.rkt"
         "../lang/document.rkt"
         "../lang/check-syntax.rkt"
         "../lang/indent.rkt"
         "../lang/lexer.rkt") ; for token

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
           'hoverProvider #t
           'definitionProvider #t
           'documentSymbolProvider #t
           'documentLinkProvider #t
           'documentFormattingProvider #t
           'documentRangeFormattingProvider #t)))

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

(define (text-document/definition ws params)
  (match-define
    (TextDocumentPositionParams
     #:textDocument (TextDocumentIdentifier #:uri uri)
     #:position (Position #:line line #:character char))
    params)
  (define doc (send ws request-traced uri))
  (define doc-text (document->text% doc))
  (define bindings (send (document:trace doc) get-bindings))
  (define pos (line/char->pos doc-text line char))
  (define declaration (interval-map-ref bindings pos #f))
  (match declaration
    [#f 'null]
    [(binding start end type)
     (Location
      #:uri uri
      #:range
      (Range
       #:start (pos->Position doc-text start)
       #:end (pos->Position doc-text end)))]))

(define (text-document/document-symbol ws params)
  (match-define
    (TextDocumentSymbolParams
     #:textDocument (TextDocumentIdentifier #:uri uri))
    params)
  (define doc (send ws request-tokenized uri))
  (define doc-text (document->text% doc))
  (define filtered-tokens
    (filter
     (lambda (tok)
       (match-define (token lexeme type data start end mode diff) tok)
       (match type
         ['constant #t]
         ['string #t]
         ['symbol #t]
         [_ #f]))
     (document:tokens doc)))
  (map
   (lambda (tok)
     (match-define (token lexeme type data start end mode diff) tok)
     (SymbolInformation
      #:name lexeme
      #:kind (match type
               ['constant SymbolKindConstant]
               ['string SymbolKindString]
               ['symbol SymbolKindVariable])
      #:location
      (Location
       #:uri uri
       #:range (pos/pos->Range doc-text (sub1 start) (sub1 end)))))
     filtered-tokens))

(define (text-document/document-link ws params)
  (match-define
    (DocumentLinkParams
     #:textDocument (TextDocumentIdentifier #:uri uri))
    params)
  (define doc (send ws request-traced uri))
  (define doc-text (document->text% doc))
  (define document-links
    (append
     (send (document:trace doc) get-require-locations)
     (send (document:trace doc) get-documentation)))

  (map
   (lambda (ln)
     (match-define (link start end target) ln)
     (DocumentLink
      #:range (pos/pos->Range doc-text start end)
      #:target target))
   document-links))

(define (text-document/formatting ws params)
  (match-define
    (DocumentFormattingParams
     #:textDocument (TextDocumentIdentifier #:uri uri))
    params)
  (define doc (send ws request uri))
  (define doc-text (document->text% doc))
  (list
   (TextEdit
    #:range (pos/pos->Range doc-text 0 (string-length (document:text doc)))
    #:newText (indent (document:text doc)))))

(define (text-document/range-formatting ws params)
  (match-define
    (DocumentRangeFormattingParams
     #:textDocument (TextDocumentIdentifier #:uri uri)
     #:range (Range #:start (Position #:line start-ln #:character start-ch)
                    #:end (Position #:line end-ln #:character end-ch)))
    params)
  (define doc (send ws request uri))
  (define doc-text (document->text% doc))
  (define start (line/char->pos doc-text start-ln start-ch))
  (define end (line/char->pos doc-text end-ln end-ch))
  (list
   (TextEdit
    #:range (pos/pos->Range doc-text 0 (string-length (document:text doc)))
    #:newText (indent (document:text doc) start end))))

(define (racket/indent ws params)
  (match-define
    (hash-table ['textDocument (TextDocumentIdentifier #:uri uri)]
                ['line line])
    params)
  (define doc (send ws request uri))
  (compute-amount-to-indent (document:text doc) line))

(provide (all-defined-out))
