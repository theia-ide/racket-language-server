#lang racket/base
(require racket/class
         "lang/workspace.rkt"
         "protocol/jsonrpc.rkt"
         "protocol/dispatch.rkt"
         "protocol/notifications.rkt")

(define (main-loop [ws (new workspace%
                            [on-tokenize on-tokenize]
                            [on-trace on-trace])])
  (define message (read-message))
  (process-message ws message)
  (main-loop ws))

(module+ main
  (main-loop))
