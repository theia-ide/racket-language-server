#lang racket/base
(require racket/list
         racket/contract/base
         "json-util.rkt")

;; Position
(define-json-expander Position
  [line exact-nonnegative-integer?]
  [character exact-nonnegative-integer?])

(define (make-Position line character)
  (Position #:line line
            #:character character))

;; Range
(define-json-expander Range
  [start any/c]
  [end any/c])

(define (make-Range start-line start-character end-line end-character)
  (Range #:start (make-Position start-line start-character)
         #:end (make-Position end-line end-character)))

;; Location
(define-json-expander Location
  [uri string?]
  [range any/c])

(define (make-Location uri range)
  (Location #:uri uri #:range range))

;; Diagnostic
(define DiagnosticSeverityError 1)
(define DiagnosticSeverityWarning 2)
(define DiagnosticSeverityInformation 3)
(define DiagnosticSeverityHint 4)

(define-json-expander DiagnosticRelatedInformation
  [location any/c]
  [message string?])

(define (make-DiagnosticRelatedInformation location message)
  (DiagnosticRelatedInformation
   #:location location
   #:message message))

(define-json-expander Diagnostic
  [range any/c]
  [message string?]
  [severity exact-nonnegative-integer?]
  [code string?]
  [source string?]
  [relatedInformation list?])

(define (make-Diagnostic range message
                         #:severity [severity DiagnosticSeverityError]
                         #:code [code ""]
                         #:source [source "Racket"]
                         #:relatedInformation [relatedInformation empty])
  (Diagnostic #:range range
              #:message message
              #:severity severity
              #:code source
              #:source source
              #:relatedInformation relatedInformation))

;; Command
(define-json-expander Command
  [title string?]
  [command string?]
  [arguments list?])

(define (make-Command title command [arguments empty])
  (Command #:title title
           #:command command
           #:arguments arguments))

;; TextEdit
(define-json-expander TextEdit
  [range any/c]
  [newText string?])

(define (make-TextEdit range newText)
  (TextEdit #:range range
            #:newText newText))

;; TextDocumentEdit
(define-json-expander TextDocumentEdit
  [textDocument any/c]
  [edits list?])

(define (make-TextDocumentEdit uri edits)
  (TextDocumentEdit #:textDocument (make-TextDocumentIdentifier uri)
                    #:edits edits))

;; WorkspaceEdit
(define-json-expander WorkspaceEdit
  [documentChanges list?])

(define (make-WorkspaceEdit documentChanges)
  (WorkspaceEdit #:documentChanges documentChanges))

;; TextDocumentIdentifier
(define-json-expander TextDocumentIdentifier
  [uri string?])

(define (make-TextDocumentIdentifier uri)
  (TextDocumentIdentifier #:uri uri))

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

(provide (all-defined-out))
