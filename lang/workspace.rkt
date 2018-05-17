#lang racket/base
(require racket/class
         racket/match
         racket/string
         framework
         "check-syntax.rkt"
         "worker.rkt")

(struct document (text trace worker))

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

    (define/public (add-doc uri text)
      (define doc-text (new racket:text%))
      (send doc-text insert text 0)
      (define worker (start-check-syntax (uri->path uri)
                                         text
                                         (lambda (trace)
                                           (report uri doc-text trace))))
      (define doc (document doc-text #f worker))
      (hash-set! docs (string->symbol uri) doc)
      (change uri doc-text))

    (define/public (remove-doc uri)
      (match (get-doc uri)
        [(document _ _ worker)
         (kill-worker worker)
         (hash-remove! docs (string->symbol uri))]
        [_ #f]))

    (define/public (update-doc uri text start end)
      (match (get-doc uri)
        [(document doc-text _ worker)
         (send doc-text insert text start end)

         (define new-text (send doc-text get-text))
         (worker-send worker new-text)
         (change uri doc-text)]
        [_ #f]))

    (define/public (replace-doc uri text)
      (match (get-doc uri)
        [(document doc-text _ worker)
         (send doc-text erase)
         (send doc-text insert text 0)

         (worker-send worker text)
         (change uri doc-text)]
        [_ #f]))

    (define/public (get-doc-text uri)
      (match (get-doc uri)
        [(document doc-text _ _) doc-text]
        [_ #f]))

    ;; Always returns the most recent available trace.
    ;; If non is available it will block and memoize the result.
    (define/public (get-doc-trace uri)
      (match (get-doc uri)
        [(document doc-text doc-trace worker)
         (define new-doc-trace (worker-receive worker doc-trace))
         (hash-set! docs (string->symbol uri)
                    (document doc-text new-doc-trace worker))
         new-doc-trace]
        [_ #f]))

    (define/public (open-doc-text uri)
      (define doc-text (get-doc-text uri))
      (if doc-text (send doc-text get-text) #f))

    (define/public (open-doc uri)
      (define doc-text (get-doc-text uri))
      (define doc-trace (get-doc-trace uri))
      (values doc-text doc-trace))

  ))

(module+ test
  (require rackunit)

  (define ws (new workspace%
                  [change (lambda (uri text) #f)]
                  [report (lambda (uri text trace) #f)]))

  (define uris '("file:///home/user/file.txt"
                 "file:///home/user/other.txt"))

  (define texts '("hello world!" "some more text!"))

  (check-equal? (send ws open-doc-text (car uris)) #f)

  (send ws add-doc (car uris) (car texts))
  (check-equal? (send ws open-doc-text (car uris)) (car texts))

  (send ws replace-doc (car uris) (car (cdr texts)))
  (check-equal? (send ws open-doc-text (car uris)) (car (cdr texts)))

  (check-true (is-a? (send ws get-doc-text (car uris)) racket:text%))
  (check-true (is-a? (send ws get-doc-trace (car uris)) build-trace%))

  (send ws remove-doc (car uris))
  (check-equal? (send ws open-doc-text (car uris)) #f)

  )

(provide workspace%)
