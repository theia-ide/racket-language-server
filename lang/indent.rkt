#lang racket/base
(require racket/class
         racket/list
         racket/string
         framework
         "document.rkt")

(module+ test
  (require rackunit)

  (define (space-char? x) (char=? x #\space))

  (define (string-indents str)
    (for/list ([line (in-list (string-split str "\n"))])
      (define len (length (takef (string->list line) space-char?)))
      (and (exact-positive-integer? len) len)))

  (define racket-str #<<RACKET
#lang racket/base
(a
(b
(c
(d
))))
RACKET
    )

  (define electron-str #<<ELECTRON
#lang electron
// comment line
/* multiline comment */
module MOD {
port PORT
net NET
OTHERMOD {PORT, OTHERPORT=NET}
}
ELECTRON
    ))

(define (get-info input symbol default)
  (define input-port (if (string? input) (open-input-string input) input))
  ((read-language input-port) symbol default))

(define (get-indenter text)
  (with-handlers ([exn? (lambda (e) #f)])
    (get-info text 'drracket:indentation #f)))

(define indent:text%
  (class racket:text%
    (init-field indenter)
    (define/augment (compute-amount-to-indent [posn 0])
      (if indenter
          (indenter this posn)
          (send this compute-racket-amount-to-indent posn)))
    (super-new)))

(define (string->indent:text% text)
  (define tbox (new indent:text% [indenter (get-indenter text)]))
  (send tbox insert text 0)
  tbox)

(define (indent text [start 0] [end (string-length text)])
  (define tbox (string->indent:text% text))
  (send tbox tabify-selection start end)
  (send tbox get-text))

(module+ test
  (check-equal?
   (string-indents (indent racket-str))
   '(#f #f 1 2 3 4))

  ;; Disable electron dsl tests in CI
  (when (not (getenv "CI"))
    (check-equal?
     (string-indents (indent electron-str))
     '(#f #f #f #f 2 2 2 #f))))

(define (compute-amount-to-indent text line)
  (define tbox (string->indent:text% text))
  (define pos (send tbox line-start-position line))
  (send tbox compute-amount-to-indent pos))

(module+ test
  (check-equal?
   (compute-amount-to-indent racket-str 2) 1)

  ;; Disable electron dsl tests in CI
  (when (not (getenv "CI"))
    (check-equal?
     (compute-amount-to-indent electron-str 4) 2)))

(provide indent compute-amount-to-indent)
