#lang racket/base

(define (empty-queue msg)
  (define msg* (thread-try-receive))
  (if msg* (empty-queue msg*) msg))

(define (start-worker work)
  (define return-to (current-thread))
  (define (do-work)
    (define msg (empty-queue (thread-receive)))
    (define res (work msg))
    (thread-send return-to res)
    (do-work))
  (thread do-work))

(provide (all-defined-out))

(module+ test
  (require rackunit)

  (define (work msg)
    (add1 msg))

  (define thd (start-worker work))
  (thread-send thd 2)
  (check-equal? (thread-receive) 3)

  (thread-send thd 8)
  (thread-send thd 4)
  (thread-send thd 10)
  (check-equal? (empty-queue (thread-receive)) 11))
