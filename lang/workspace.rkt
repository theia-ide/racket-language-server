#lang racket/base
(require racket/class
         racket/match
         racket/string
         framework
         "check-syntax.rkt")

(struct document (text trace worker))

(define (uri-is-path? str)
  (string-prefix? str "file://"))

(define (uri->path uri)
  (substring uri 7))

(define workspace%
  (class object%
    (super-new)
    (init-field [docs (make-hasheq)])

    (define (get-doc uri)
      (unless (uri-is-path? uri)
        (error 'document-symbol "uri is not a path"))
      (hash-ref docs (string->symbol uri) #f))

    (define/public (add-doc uri text)
      (define doc-text (new racket:text%))
      (send doc-text insert text 0)
      (define worker (start-check-syntax (uri->path uri) text))
      (define doc (document doc-text #f worker))
      (hash-set! docs (string->symbol uri) doc))

    (define/public (remove-doc uri)
      (match (get-doc uri)
        [(document _ _ worker)
         (kill-check-syntax worker)
         (hash-remove! docs (string->symbol uri))]
        [_ #f]))

    (define/public (update-doc uri text start end)
      (match (get-doc uri)
        [(document doc-text _ worker)
         (send doc-text insert text start end)

         (define text (send doc-text get-text))
         (send-check-syntax worker text)]
        [_ #f]))

    (define/public (replace-doc uri text)
      (match (get-doc uri)
        [(document doc-text _ worker)
         (send doc-text erase)
         (send doc-text insert text 0)

         (send-check-syntax worker text)]
        [_ #f]))

    (define/public (get-doc-text uri)
      (match (get-doc uri)
        [(document doc-text _ _) doc-text]
        [_ #f]))

    (define/public (get-doc-trace uri)
      (match (get-doc uri)
        [(document doc-text doc-trace worker)
         (define new-doc-trace (receive-check-syntax worker doc-trace))
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

  (define ws (new workspace%))

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
