#lang racket/base
(require racket/exn
         racket/match
         "jsonrpc.rkt"
         "methods.rkt")

(define (process-message state msg)
    (match msg
      ;; Request
      [(JsonRpcMessage #:id id #:method method)
       (define params (hash-ref msg 'params hasheq))
       (with-handlers ([exn:fail? (send-internal-server-error id method)]
                       [exn:misc:match? (send-invalid-params-error id method)])
         (process-request state id method params))]
      ;; Notification
      [(JsonRpcMessage #:method method)
       (define params (hash-ref msg 'params hasheq))
       (with-handlers ([exn:fail? (send-internal-server-error 'null method)]
                       [exn:misc:match? (send-invalid-params-error 'null method)])
         (process-notification state method params))]
      ;; Invalid message with id
      [(JsonRpcMessage #:id id)
       (send-invalid-request-error id)]
      ;; Invalid message without id
      [_ (send-invalid-request-error 'null)]))

(define (process-request state id method params)
  (define (execute request)
    (write-message
     (success-response id (request state params))))
  (match method
    ["initialize" (execute lsp/initialize)]
    ["shutdown" (execute lsp/shutdown)]
    ["textDocument/hover" (execute text-document/hover)]
    ["textDocument/documentSymbol" (execute text-document/document-symbol)]
    ["textDocument/documentLink" (execute text-document/document-link)]
    [_ (send-method-not-found-error id method)]))

(define (process-notification state method params)
  (match method
    ["$/cancelRequest" (lsp/cancel-request state params)]
    ["initialized" (lsp/initialized state params)]
    ["exit" (lsp/exit state params)]
    ["textDocument/didOpen" (text-document/did-open state params)]
    ["textDocument/didClose" (text-document/did-close state params)]
    ["textDocument/didChange" (text-document/did-change state params)]
    [_ (send-method-not-found-error 'null method)]))

(define (send-invalid-request-error id)
  (eprintf "Invalid request\n")
  (write-message
   (error-response id INVALID-REQUEST
                   "The JSON sent is not a valid request object.")))

(define (send-method-not-found-error id method)
  (eprintf "Method not found ~v\n" method)
  (define err (format "The method ~v was not found." method))
  (write-message
   (error-response id METHOD-NOT-FOUND err)))

(define ((send-invalid-params-error id method) exn)
  (eprintf "Caught exn:misc:match? in request ~v\n~a\n" method (exn->string exn))
  (define err (format "invalid params for method ~v" method))
  (write-message
   (error-response id INVALID-PARAMS err)))

(define ((send-internal-server-error id method) exn)
  (eprintf "Caught exn in request ~v\n~a\n" method (exn->string exn))
  (define err (format "internal error in method ~v" method))
  (write-message
   (error-response id INTERNAL-ERROR err)))

(provide process-message)
