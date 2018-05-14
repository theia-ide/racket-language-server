#lang racket/base
(require racket/match
         racket/port
         json)

;; Defined by JSON RPC
(define PARSE-ERROR -32700)
(define INVALID-REQUEST -32600)
(define METHOD-NOT-FOUND -32601)
(define INVALID-PARAMS -32602)
(define INTERNAL-ERROR -32603)
(define SERVER-ERROR-START -32099)
(define SERVER-ERROR-END -32000)
(define SERVER-NOT-INITIALIZED -32002)
(define UNKNOWN-ERROR-CODE -32001)

;; Defined by LSP protocol
(define REQUEST-CANCELLED -32800)

;; Read a message from in
(define (read-message [in (current-input-port)])
  (match (read-line in 'return-linefeed)
    ["" (with-handlers ([exn:fail:read? (λ (exn) 'parse-json-error)])
          (read-json in))]
    [(? eof-object?) eof]
    [_ (read-message in)]))

;; Write a message to out
(define (write-message msg [out (current-output-port)])
  (define null-port (open-output-nowhere))
  (write-json msg null-port)
  (define content-length (file-position null-port))
  (fprintf out "Content-Length: ~a\r\n\r\n" content-length)
  (write-json msg out)
  (flush-output out))

;; Constructor for a notification
(define (send-notification method params)
  (define notification
    (hasheq 'jsonrpc "2.0"
            'method method
            'params params))
  (write-message notification))

;; Constructor for a response object representing success.
(define (success-response id result)
  (hasheq 'jsonrpc "2.0"
          'id id
          'result result))

;; Constructor for a response object representing failure.
(define (error-response id code message [data #f])
  (define err
    (hasheq 'code code
            'message message))
  (define err*
    (if data err (hash-set err 'data data)))
  (hasheq 'jsonrpc "2.0"
          'id id
          'error err*))

(provide (all-defined-out))
