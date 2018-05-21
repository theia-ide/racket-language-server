#lang racket/base
(require racket/class
         racket/function
         racket/match
         racket/string
         racket/gui/base
         "document.rkt"
         "worker.rkt")

(define current-doc-semaphore (make-semaphore 1))
(struct doc-entry (version doc-text worker))
(struct current-document (changed tokenized traced traced-no-error))

(define (uri-is-path? str)
  (string-prefix? str "file://"))

(define workspace%
  (class object%
    (super-new)
    (init-field
     [on-change #f]
     [on-tokenize #f]
     [on-trace #f]
     [doc-entries (make-hasheq)]
     [current-docs (make-hasheq)])

    (define (get-doc-entry uri)
      (hash-ref doc-entries (string->symbol uri)))

    (define (get-current-doc uri)
      (hash-ref current-docs (string->symbol uri)))

    (define (update-doc-entry uri update-procedure)
      (match-define (doc-entry version doc-text worker) (get-doc-entry uri))

      (update-procedure doc-text)

      (define new-version (add1 version))
      (define new-text (send doc-text get-text))
      (define entry (doc-entry new-version doc-text worker))
      (hash-set! doc-entries (string->symbol uri) entry)

      (define doc (document uri new-version new-text))
      (emit-on-change doc)

      (define doc-tokenized (make-tokenized-document doc))
      (emit-on-tokenize doc-tokenized)

      (worker-send worker doc-tokenized))

    (define (update-current-docs doc)
      (define uri (string->symbol (document:uri doc)))
      (call-with-semaphore
       current-doc-semaphore
       (lambda ()
         (match-define (current-document changed tokenized traced traced-no-error)
           (hash-ref current-docs uri))
         (define new-current-document
           (match doc
             [(document _ _ _)
              (current-document doc tokenized traced traced-no-error)]
             [(tokenized-document _ _ _ _)
              (current-document changed doc traced traced-no-error)]
             [(traced-document _ _ _ _ trace)
              (if (has-syntax-error doc)
                  (current-document changed tokenized doc traced-no-error)
                  (current-document changed tokenized doc doc))]))
         (hash-set! current-docs uri new-current-document))))

    ;; Events
    (define (emit doc cb)
      (update-current-docs doc)
      (when cb (cb doc)))

    (define/public (set-on-change cb) (set! on-change cb))
    (define/public (set-on-tokenize cb) (set! on-tokenize cb))
    (define/public (set-on-trace cb) (set! on-trace cb))
    (define (emit-on-change doc) (emit doc on-change))
    (define (emit-on-tokenize doc) (emit doc on-tokenize))
    (define (emit-on-trace doc) (emit doc on-trace))

    ;; Background thread
    (define (start-worker)
      (define (work doc-tokenized)
        (define doc-traced (make-traced-document doc-tokenized))
        (emit-on-trace doc-traced)
        doc-traced)
      (make-worker work))

    (define (receive-document worker version)
      (define doc (worker-receive worker #f))
      (if (>= (document:version doc) version)
          doc
          (receive-document worker version)))


    ;; Notification interface
    (define/public (notify-open uri text)
      (unless (uri-is-path? uri)
        (error 'document-symbol "uri is not a path"))
      (define entry (doc-entry 0 (new text%) (start-worker)))
      (define docs (current-document #f #f #f #f))
      (hash-set! doc-entries (string->symbol uri) entry)
      (hash-set! current-docs (string->symbol uri) docs)
      (update-doc-entry uri
                        (lambda (doc-text)
                          (send doc-text insert text 0))))

    (define/public (notify-close uri)
      (match (get-doc-entry uri)
        [(doc-entry _ _ worker)
         (kill-worker worker)
         (hash-remove! doc-entries (string->symbol uri))
         (hash-remove! current-docs (string->symbol uri))]))

    (define/public (notify-update uri text start end)
      (update-doc-entry uri
                        (lambda (doc-text)
                          (send doc-text insert text start end))))

    (define/public (notify-replace uri text)
      (update-doc-entry uri
                        (lambda (doc-text)
                          (send doc-text erase)
                          (send doc-text insert text 0))))

    ;; Request interface
    (define/public (request uri)
      (match-define (current-document changed _ _ _)
        (get-current-doc uri))
      changed)

    (define/public (request-tokenized uri)
      (match-define (current-document _ tokenized _ _)
        (get-current-doc uri))
      tokenized)

    (define/public (request-traced uri)
      (match-define (doc-entry version _ worker)
        (get-doc-entry uri))
      (match-define (current-document _ _ traced _)
        (get-current-doc uri))
      (if (and traced (eq? (document:version traced) version))
          traced
          (receive-document worker version)))

    ))

(provide workspace%)

(module+ test
  (require rackunit)

  (define ws (new workspace%))

  (define uris '("file:///home/user/file.txt"
                 "file:///home/user/other.txt"))

  (define syntaxes
    (list #'(displayln "hello world!")
          #'(displayln "bye world!")))

  (define (syntax->text syn)
    (format "#lang racket\n\n~a" (syntax->datum syn)))

  (define texts (map syntax->text syntaxes))

  (define (check-document doc curi cversion ctext)
    (match-define (document uri version text) doc)
    (check-equal? uri curi)
    (check-equal? version cversion)
    (check-equal? text ctext))

  (define (check-tokenized-document doc curi cversion ctext)
    (match-define (tokenized-document uri version text _) doc)
    (check-equal? uri curi)
    (check-equal? version cversion)
    (check-equal? text ctext))

  (define (check-traced-document doc curi cversion ctext)
    (match-define (traced-document uri version text _ _) doc)
    (check-equal? uri curi)
    (check-equal? version cversion)
    (check-equal? text ctext))

  (define (register-checks ws curi cversion ctext)
    (send ws set-on-change
          (lambda (doc) (check-document doc curi cversion ctext)))
    (send ws set-on-tokenize
          (lambda (doc) (check-tokenized-document doc curi cversion ctext)))
    (send ws set-on-trace
          (lambda (doc) (check-traced-document doc curi cversion ctext))))

  (define (check-notify ws method uri version text)
    (register-checks ws uri version text)
    (dynamic-send ws method uri text)
    (check-document (send ws request uri) uri version text)
    (check-tokenized-document (send ws request-tokenized uri) uri version text)
    (check-traced-document (send ws request-traced uri) uri version text))

  (check-notify ws 'notify-open (car uris) 1 (car texts))
  (check-notify ws 'notify-open (car (cdr uris)) 1 (car (cdr texts)))
  (check-notify ws 'notify-replace (car uris) 2 (car (cdr texts)))

  (send ws notify-close (car uris))
  (send ws notify-close (car (cdr uris)))
  )

(provide workspace%)
