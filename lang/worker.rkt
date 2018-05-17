#lang racket/base
(require racket/async-channel
         racket/match)

(struct worker (thrd ch))

(define (make-worker work)
  (define return-to (make-async-channel))
  (define (do-work)
    (define msg (empty-queue thread-try-receive (thread-receive)))
    (define res (work msg))
    (async-channel-put return-to res)
    (do-work))
  (worker (thread do-work) return-to))

(define (worker-receive w last-msg)
  (define ((try-receive ch))
    (async-channel-try-get ch))

  (match-define (worker _ ch) w)
  (define msg (if last-msg last-msg (async-channel-get ch)))
  (empty-queue (try-receive ch) msg))

(define (worker-send w msg)
  (match-define (worker thrd _) w)
  (thread-send thrd msg))

(define (kill-worker w)
  (match-define (worker thrd _) w)
  (kill-thread thrd))

(define (empty-queue try-receive last-msg)
  (define msg (try-receive))
  (if msg (empty-queue try-receive msg) last-msg))

(provide make-worker worker-receive worker-send kill-worker)

(module+ test
  (require rackunit)

  (define (work msg)
    (add1 msg))

  (define wrk (make-worker work))
  (worker-send wrk 2)
  (check-equal? (worker-receive wrk #f) 3)

  (worker-send wrk 8)
  (worker-send wrk 4)
  (worker-send wrk 10)
  (check-equal? (worker-receive wrk #f) 11))
