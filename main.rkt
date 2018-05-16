#lang racket/base
(require racket/class
         "lang/workspace.rkt"
         "protocol/jsonrpc.rkt"
         "protocol/dispatch.rkt")

(define (main-loop [ws (new workspace%)])
  (define message (read-message))
  (process-message ws message)
  (main-loop ws))

(module+ main
  (main-loop))
