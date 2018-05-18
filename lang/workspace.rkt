#lang racket/base
(require racket/class
         racket/match
         racket/string
         framework
         "check-syntax.rkt"
         "lexer.rkt"
         "worker.rkt")

(struct document (text tokens trace worker))

(define (uri-is-path? str)
  (string-prefix? str "file://"))

(define (uri->path uri)
  (substring uri 7))

(define workspace%
  (class object%
    (super-new)
    (init-field change report [docs (make-hasheq)])

    (define (get-doc uri)
      (unless (uri-is-path? uri)
        (error 'document-symbol "uri is not a path"))
      (hash-ref docs (string->symbol uri) #f))

    (define (tokenize text)
      (apply-tokenizer-maker
       (compose lang-tokenizer make-tokenizer)
       text))

    (define (do-change uri text doc-text doc-trace worker)
      ;; Tokenize text
      (define tokens (tokenize text))

      ;; Update document
      (define doc (document doc-text tokens doc-trace worker))
      (hash-set! docs (string->symbol uri) doc)

      ;; Send text to worker (make sure this happens after tokenization)
      (worker-send worker text)

      ;; Emit change event (hide trace and worker)
      ;; If trace is required the report event should be used.
      (change uri doc-text tokens))

    (define ((do-report uri ws reporter) doc-trace)
      (match-define (document doc-text doc-tokens _ _) (get-doc uri))
      (reporter uri doc-text doc-tokens doc-trace))

    (define/public (add-doc uri text)
      (define doc-text (new racket:text%))
      (send doc-text insert text 0)
      (define worker (start-check-syntax
                      (uri->path uri)
                      (do-report uri this report)))
      (do-change uri text doc-text #f worker))

    (define/public (remove-doc uri)
      (match (get-doc uri)
        [(document _ _ _ worker)
         (kill-worker worker)
         (hash-remove! docs (string->symbol uri))]
        [_ #f]))

    (define/public (update-doc uri text start end)
      (displayln "updating" (current-error-port))
      (match (get-doc uri)
        [(document doc-text _ doc-trace worker)
         (send doc-text insert text start end)

         (define new-text (send doc-text get-text))
         (do-change uri new-text doc-text doc-trace worker)]
        [_ #f]))

    (define/public (replace-doc uri text)
      (match (get-doc uri)
        [(document doc-text doc-tokens doc-trace worker)
         (send doc-text erase)
         (send doc-text insert text 0)

         (do-change uri text doc-text doc-trace worker)]
        [_ #f]))

    (define/public (get-doc-text uri)
      (match (get-doc uri)
        [(document doc-text _ _ _) doc-text]
        [_ #f]))

    ;; Always returns the most recent available trace.
    ;; If non is available it will block and memoize the result.
    (define/public (get-doc-trace uri)
      (match (get-doc uri)
        [(document doc-text doc-tokens doc-trace worker)
         (define new-doc-trace (worker-receive worker doc-trace))
         (hash-set! docs (string->symbol uri)
                    (document doc-text doc-tokens new-doc-trace worker))
         new-doc-trace]
        [_ #f]))

    (define/public (get-doc-tokens uri)
      (match (get-doc uri)
        [(document _ doc-tokens _ _) doc-tokens]
        [_ #f]))

    (define/public (open-doc-text uri)
      (define doc-text (get-doc-text uri))
      (if doc-text (send doc-text get-text) #f))

    (define/public (open-doc uri)
      (define doc-text (get-doc-text uri))
      (define doc-tokens (get-doc-tokens uri))
      (define doc-trace (get-doc-trace uri))
      (values doc-text doc-tokens doc-trace))

  ))

(module+ test
  (require rackunit)

  (define ws (new workspace%
                  [change (lambda (uri doc-text doc-tokens) #f)]
                  [report (lambda (uri doc-text doc-tokens doc-trace) #f)]))

  (define uris '("file:///home/user/file.txt"
                 "file:///home/user/other.txt"))

  (define syntaxes
    (list #'(displayln "hello world!")
          #'(displayln "bye world!")))

  (define (syntax->text syn)
    (format "#lang racket\n\n~a" (syntax->datum syn)))

  (define texts (map syntax->text syntaxes))

  (check-equal? (send ws open-doc-text (car uris)) #f)

  (send ws add-doc (car uris) (car texts))
  (check-equal? (send ws open-doc-text (car uris)) (car texts))

  (send ws replace-doc (car uris) (car (cdr texts)))
  (check-equal? (send ws open-doc-text (car uris)) (car (cdr texts)))

  (check-true (is-a? (send ws get-doc-text (car uris)) racket:text%))
  (check-true (list? (send ws get-doc-tokens (car uris))))
  (check-true (is-a? (send ws get-doc-trace (car uris)) build-trace%))

  (send ws remove-doc (car uris))
  (check-equal? (send ws open-doc-text (car uris)) #f)

  )

(provide workspace%)
