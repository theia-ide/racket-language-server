#lang racket/base
(require racket/class
         racket/list
         racket/match
         racket/set
         data/interval-map
         drracket/check-syntax
         syntax/modread
         "worker.rkt")

(struct exception (code msg srclocs))
(struct warning (code msg srclocs))

(define (exn->exception e)
  ;(define-values (code _ _ _ _ _ _ _) (struct-type-info e))
  (define msg (exn-message e))
  (define srclocs ((exn:srclocs-accessor e) e))
  (exception ("exn" msg srclocs)))

(define build-trace%
  (class (annotations-mixin object%)
    (super-new)
    (init-field path)

    (define errors empty)
    (define warnings empty)

    (define semantic-coloring (make-interval-map))
    (define hovers (make-interval-map))

    ;; pos -> (set pos ...)
    (define symbol-declarations (make-interval-map))
    ;; pos -> pos
    (define symbol-bindings (make-interval-map))

    (define/public (add-error err)
      (set! errors (cons err errors))
      (void))

    (define/public (add-warning warn)
      (set! warnings (cons warn warnings))
      (void))

    (define/public (get-hovers) hovers)

    (define/override (syncheck:find-source-object stx)
      ;; Skip annotations if source-object if it's source location is
      ;; from a different file.
      (and (equal? path (syntax-source stx))
           path))

    (define/override (syncheck:add-text-type source-obj start end text-type)
      ;; TODO
      (void))

    (define/override (syncheck:add-require-open-menu source-obj start end path)
      ;; TODO: documentLink
      (void))

    (define/override (syncheck:add-docs-menu
                      source-obj start end id label path tag)
      ;; TODO: documentLink
      (void))

    (define/override (syncheck:add-arrow/name-dup/pxpy
                      method start-source-obj start-left start-right
                      start-px start-py end-source-obj end-left end-right
                      end-px end-py actual? phase-level require-arrow
                      name-dup?)
      ;; Mapping from doc declaration to set of bindings.
      (define prev-bindings (interval-map-ref symbol-declarations start-left set))
      (define new-bindings (set-add prev-bindings (cons end-left end-right)))
      (interval-map-set! symbol-declarations start-left start-right new-bindings)
      ;; Mapping from binding to declaration.
      (define new-decl (cons start-left start-right))
      (interval-map-set! symbol-bindings end-left end-right new-decl)
      ;; Each decl is considered to be bound to itself
      ;; i.e. it is a key in map from doc positions to declarations.
      (interval-map-set! symbol-bindings start-left start-right new-decl)
      (void))

    (define/override (syncheck:add-tail-arrow
                      from-source-obj from-pos to-source-obj to-pos)
      ;; TODO
      (void))

    (define/override (syncheck:add-mouse-over-status
                      source-obj start finish text)
      (interval-map-set! hovers start finish text)
      (void))

    (define/override (syncheck:add-prefixed-require-reference
                      req-src req-pos-left req-pos-right)
      ;; TODO
      (void))

    (define/override (syncheck:add-unused-require
                      req-src req-pos-left req-pos-right)
      (add-warning (warning 'warn:unused-require "Unused require."
                            ;; line and column unknown
                            (srcloc path #f #f req-pos-left
                                    (- req-pos-right req-pos-left))))
      (void))

    (define/override (syncheck:add-jump-to-definition
                      source-obj start end id filename submods)
      ;; TODO
      (void))

    (define/override (syncheck:add-definition-target
                      source-obj start end style-name)
      ;; TODO
      (void))

    ;; style-name: any/c, mode: any/c
    (define/override (syncheck:color-range
                      source-obj start finish style-name)
      (interval-map-set! semantic-coloring start finish style-name)
      (void))

    ))

(define (check-syntax path text)
  (define ns (make-base-namespace))
  (define trace (new build-trace% [path path]))
  (match-define-values (src-dir _ #f)
                       (split-path path))
  (define-values (add-syntax done)
    (make-traversal ns path))
  (define in (open-input-string text))
  (port-count-lines! in)
  (parameterize ([current-annotations trace]
                 [current-namespace ns]
                 [current-load-relative-directory src-dir])
    (with-handlers ([exn?
                     (lambda (e)
                       (send trace add-error (exn->exception e)))])
      (define stx (with-module-reading-parameterization
                    (λ () (read-syntax text in))))
      (add-syntax (expand stx)))
    (done))
  trace)


(define (start-check-syntax path text)
  (define (work msg)
    (check-syntax path msg))
  (define worker (start-worker work))
  worker)

(define (send-check-syntax worker text)
  (thread-send worker text))

(define (receive-check-syntax worker trace)
  (empty-queue (if trace trace (thread-receive))))

(define (kill-check-syntax worker)
  (kill-thread worker))

(provide (all-defined-out))
