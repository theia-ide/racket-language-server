#lang racket/base
(require racket/class
         racket/match
         racket/string
         framework
         rackunit)

(struct document (text trace dirty))

(define (uri-is-path? str)
  (string-prefix? str "file://"))

(define (uri->path uri)
  (substring uri 7))

(define workspace%
  (class object%
    (super-new)
    (init-field [docs (make-hasheq)])

    (define/public (add-doc uri text)
      (define doc-text (new racket:text%))
      (send doc-text insert text 0)
      (define doc-trace #f) ;(new build-trace% [src text]))
      (define doc (document doc-text doc-trace #t))
      (hash-set! docs (string->symbol uri) doc)
      doc)

    (define (get-doc uri)
      (unless (uri-is-path? uri)
        (error 'document-symbol "uri is not a path"))
      (hash-ref docs (string->symbol uri) #f))

    (define/public (get-doc-text uri)
      (match (get-doc uri)
        [(document doc-text _ _) doc-text]
        [_ #f]))

    (define (get-doc-trace uri)
      (match (get-doc uri)
        [(document _ doc-trace _) doc-trace]
        [_ #f]))

    (define (set-dirty uri)
      (match (get-doc uri)
        [(document doc-text doc-trace _)
         (hash-set! docs (string->symbol uri) (document doc-text doc-trace #t))
         #t]
        [_ #f]))

    (define/public (clear-dirty uri)
      (match (get-doc uri)
        [(document doc-text doc-trace _)
         (hash-set! docs (string->symbol uri) (document doc-text doc-trace #f))
         #t]
        [_ #f]))

    (define/public (get-dirty uri)
      (match (get-doc uri)
        [(document _ _ dirty) dirty]
        [_ #f]))

    (define/public (open-doc-text uri)
      (define doc-text (get-doc-text uri))
      (if doc-text (send doc-text get-text) #f))

    (define/public (remove-doc uri)
      (when (get-doc uri)
        (hash-remove! docs (string->symbol uri))))

    (define/public (update-doc uri text start end)
      (define doc-text (get-doc-text uri))
      (when doc-text
        (send doc-text insert text start end))
      (set-dirty uri))

    (define/public (replace-doc uri text)
      (define doc-text (get-doc-text uri))
      (when doc-text
        (send doc-text erase)
        (send doc-text insert text 0))
      (set-dirty uri))

  ))

(module+ test
  (define ws (new workspace%))

  (define uris '("file:///home/user/file.txt"
                 "file:///home/user/other.txt"))

  (define texts '("hello world!" "some more text!"))

  (check-equal? (send ws open-doc-text (car uris)) #f)
  (check-equal? (send ws get-dirty (car uris)) #f)

  (send ws add-doc (car uris) (car texts))
  (check-equal? (send ws open-doc-text (car uris)) (car texts))
  (check-equal? (send ws get-dirty (car uris)) #t)

  (send ws clear-dirty (car uris))
  (check-equal? (send ws get-dirty (car uris)) #f)

  (send ws replace-doc (car uris) (car (cdr texts)))
  (check-equal? (send ws open-doc-text (car uris)) (car (cdr texts)))
  (check-equal? (send ws get-dirty (car uris)) #t)

  (send ws remove-doc (car uris))
  (check-equal? (send ws open-doc-text (car uris)) #f))

(provide workspace%)
