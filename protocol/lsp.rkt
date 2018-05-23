#lang racket/base
(require racket/contract/base
         racket/function
         racket/list
         "json-util.rkt")

;; Position
(define-json-expander Position
  [line exact-nonnegative-integer?]
  [character exact-nonnegative-integer?])

;; Range
(define-json-expander Range
  [start any/c]
  [end any/c])

;; Location
(define-json-expander Location
  [uri string?]
  [range any/c])

;; Diagnostic
(define DiagnosticSeverityError 1)
(define DiagnosticSeverityWarning 2)
(define DiagnosticSeverityInformation 3)
(define DiagnosticSeverityHint 4)

(define-json-expander DiagnosticRelatedInformation
  [location any/c]
  [message string?])

(define-json-expander Diagnostic
  [range any/c]
  [message string?]
  [severity exact-nonnegative-integer?]
  [code string?]
  [source string?]
  [relatedInformation list?])

;; Command
(define-json-expander Command
  [title string?]
  [command string?]
  [arguments list?])

;; TextEdit
(define-json-expander TextEdit
  [range any/c]
  [newText string?])

;; TextDocumentEdit
(define-json-expander TextDocumentEdit
  [textDocument any/c]
  [edits list?])

;; WorkspaceEdit
(define-json-expander WorkspaceEdit
  [documentChanges list?])

;; TextDocumentIdentifier
(define-json-expander TextDocumentIdentifier
  [uri string?])

;; TextDocumentItem
(define-json-expander TextDocumentItem
  [uri string?]
  [languageId string?]
  [version exact-nonnegative-integer?]
  [text string?])

;; VersionedTextDocumentIdentifier
(define-json-expander VersionedTextDocumentIdentifier
  [uri string?]
  [version exact-nonnegative-integer?])

;; TextDocumentPositionParams
(define-json-expander TextDocumentPositionParams
  [textDocument any/c]
  [position any/c])

;; DocumentFilter
(define-json-expander DocumentFilter
  [language string?]
  [scheme string?]
  [pattern string?])

;; MarkupContent
(define MarkupKindPlaintext "plaintext")
(define MarkupKindMarkdown "markdown")

(define-json-expander MarkupContent
  [kind string?]
  [value string?])

;; InitializeParams
(define-json-expander InitializeParams
  [processId (or/c exact-nonnegative-integer? ((curry eq?) 'null))]
  [rootUri string?]
  [initializationOptions any/c]
  [capabilities any/c]
  [trace string?]
  [workspaceFolders (or/c list? ((curry eq?) 'null))])

(define-json-expander ClientCapabilities
  [workspace any/c]
  [textDocument any/c]
  [experimental any/c])

;; InitializeResult
(define-json-expander InitializeResult
  [capabilities any/c])

(define-json-expander ServerCapabilities
  [textDocumentSync any/c]
  [hoverProvider boolean?]
  [completionProvider any/c]
  [signatureHelpProvider any/c]
  [definitionProvider boolean?]
  [typeDefinitionProvider boolean?]
  [implementationProvider boolean?]
  [referencesProvider boolean?]
  [documentHighlightProvider boolean?]
  [documentSymbolProvider boolean?]
  [workspaceSymbolProvider boolean?]
  [codeActionProvider boolean?]
  [codeLensProvider any/c]
  [documentFormattingProvider boolean?]
  [documentRangeFormattingProvider boolean?]
  [documentOnTypeFormattingProvider any/c]
  [renameProvider boolean?]
  [documentLinkProvider any/c]
  [colorProvider boolean?]
  [executeCommandProvider any/c]
  [workspace any/c]
  [experimental any/c])

(define-json-expander TextDocumentSyncOptions
  [openClose boolean?]
  [change exact-nonnegative-integer?]
  [willSave boolean?]
  [willSaveWaitUntil boolean?]
  [save any/c])

(define TextDocumentSyncKindNone 0)
(define TextDocumentSyncKindFull 1)
(define TextDocumentSyncKindIncremental 2)

;; DidOpenTextDocumentParams
(define-json-expander DidOpenTextDocumentParams
  [textDocument any/c])

;; DidCloseTextDocumentParams
(define-json-expander DidCloseTextDocumentParams
  [textDocument any/c])

;; DidChangeTextDocumentParams
(define-json-expander DidChangeTextDocumentParams
  [textDocument any/c]
  [contentChanges list?])

;; TextDocumentContentChangeEvent
(define-json-expander TextDocumentContentChangeEvent
  [range any/c]
  [rangeLength exact-nonnegative-integer?]
  [text string?])

;; PublishDiagnosticsParams
(define-json-expander PublishDiagnosticsParams
  [uri string?]
  [diagnostics list?])

;; Hover
(define-json-expander Hover
  [contents string?]
  [range any/c])

;; TextDocumentSymbolParams
(define-json-expander TextDocumentSymbolParams
  [textDocument any/c])

;; SymbolInformation
(define-json-expander SymbolInformation
  [name string?]
  [kind exact-nonnegative-integer?]
  [location any/c])

;; SymbolKind
(define SymbolKindFile 1)
(define SymbolKindModule 2)
(define SymbolKindNamespace 3)
(define SymbolKindPackage 4)
(define SymbolKindClass 5)
(define SymbolKindMethod 6)
(define SymbolKindProperty 7)
(define SymbolKindField 8)
(define SymbolKindConstructor 9)
(define SymbolKindEnum 10)
(define SymbolKindInterface 11)
(define SymbolKindFunction 12)
(define SymbolKindVariable 13)
(define SymbolKindConstant 14)
(define SymbolKindString 15)
(define SymbolKindNumber 16)
(define SymbolKindBoolean 17)
(define SymbolKindArray 18)
(define SymbolKindObject 19)
(define SymbolKindKey 20)
(define SymbolKindNull 21)
(define SymbolKindEnumMember 22)
(define SymbolKindStruct 23)
(define SymbolKindEvent 24)
(define SymbolKindOperator 25)
(define SymbolKindTypeParameter 26)

;; DocumentLinkParams
(define-json-expander DocumentLinkParams
  [textDocument any/c])

;; DocumentLink
(define-json-expander DocumentLink
  [range any/c]
  [target string?])

(provide (all-defined-out))
