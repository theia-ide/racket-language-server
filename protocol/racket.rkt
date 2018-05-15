#lang racket/base
(require racket/class
         racket/match
         "conversion.rkt"
         "jsonrpc.rkt"
         "../lang/lexer.rkt")

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
