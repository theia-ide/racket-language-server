#lang racket/base
(require racket/function
         racket/generator
         racket/match
         racket/list
         data/interval-map
         syntax-color/module-lexer
         syntax-color/racket-lexer
         syntax-color/scribble-lexer
         "check-syntax.rkt") ; for exception

(module+ test
  (require rackunit)

  (define (check-token-transform transform tokens-in tokens-out)
    (define next-token (transform (list->producer tokens-in)))
    (check-equal?
     (append
      (for/list ([tok (in-producer next-token eof-token?)])
        tok)
      (list eof-token))
     tokens-out)))

(struct token (lexeme type data start end mode diff) #:transparent)

(define eof-token (token eof #f #f #f #f #f #f))

(define (eof-token? tok)
  (match tok
    [(token (? eof-object?) _ _ _ _ _ _) #t]
    [_ #f]))

(define (get-lexer in)
  (define offset 0)
  (define mode #f)
  (define (next-token)
    (match-define-values
     (lexeme type data start end newOffset newMode)
     (module-lexer in offset mode))
    (set! offset newOffset)
    (set! mode newMode)
    (token lexeme type data start end (mode->symbol mode) #f))
  next-token)

(define (make-tokenizer str)
  (define input-port (open-input-string str))
  (port-count-lines! input-port)
  (get-lexer input-port))

(define (apply-tokenizer-maker tokenizer val)
  (define next-token (tokenizer val))
  (for/list ([tok (in-producer next-token eof-token?)])
    tok))

(define (list->producer tokens)
  (generator ()
             (for-each yield tokens)))

(module+ test
  (check-equal?
   (for/fold ([sum 0])
             ([elem (in-producer (list->producer '(1 2 3 #f)) not)])
     (+ sum elem))
   6))

(define (mode->symbol mode)
  (define mode-proc
    (match mode
      [(? procedure?) mode]
      [(? pair?) (car mode)]))
  (cond
    [(equal? mode-proc racket-lexer) 'racket]
    [(equal? mode-proc scribble-inside-lexer) 'scribble]
    [else 'other]))

(module+ test
  (check-equal? (mode->symbol racket-lexer) 'racket)
  (check-equal? (mode->symbol scribble-inside-lexer) 'scribble)
  (check-equal? (mode->symbol identity) 'other)
  (check-equal? (mode->symbol (cons racket-lexer #f)) 'racket)
  (check-equal? (mode->symbol (cons identity #f)) 'other))

;; Splits 'other tokens starting with #lang into a 'lang-keyword
;; and 'lang-symbol token.
(define (lang-tokenizer next-token)
  (infinite-generator
   (define tok (next-token))
   (match tok
     [(token lexeme (? (curry eq? 'other)) data start end _ diff)
      (match (regexp-match #px"^(#lang)(\\s+)(.+)$" lexeme)
        [(list all lang white symbol)
         (define end-lang (+ start 5))
         (define end-white (+ end-lang (string-length white)))
         (yield (token lang 'keyword data start end-lang 'lang diff))
         (yield (token white 'white-space data end-lang end-white 'lang diff))
         (yield (token symbol 'symbol data end-white end 'lang diff))]
        [_ (yield tok)])]
     [_ (yield tok)])))

(module+ test
  (check-token-transform
   lang-tokenizer
   (list
    (token "#lang racket" 'other #f 1 13 'racket #f)
    eof-token)
   (list
    (token "#lang" 'keyword #f 1 6 'lang #f)
    (token " " 'white-space #f 6 7 'lang #f)
    (token "racket" 'symbol #f 7 13 'lang #f)
    eof-token)))

(define (skip-white next-token)
  (define (new-next-token)
    (match (next-token)
      [(token _ 'white-space _ _ _ _ _)
       (new-next-token)]
      [(token _ 'whitespace _ _ _ _ _)
       (new-next-token)]
      [tok tok]))
  new-next-token)

(module+ test
  (check-token-transform
   skip-white
   (list
    (token #f 'a #f #f #f #f #f)
    (token #f 'white-space #f #f #f #f #f)
    (token #f 'b #f #f #f #f #f)
    (token #f 'whitespace #f #f #f #f #f)
    eof-token)
   (list
    (token #f 'a #f #f #f #f #f)
    (token #f 'b #f #f #f #f #f)
    eof-token)))

;; Counts parenthesis and reclassifies the tokens of the sexp after
;; 'sexp-comment to 'comment.
(define (sexp-comment-reclassifier next-token)
  (define (is-open paren)
    (or (eq? paren '|(|) (eq? paren '|{|) (eq? paren '|[|)))
  (define state 0)
  (define (reclassify tok)
    (match tok
      [(token lexeme type paren start end mode diff)
       (cond
         [(eq? type 'sexp-comment)
          (set! state 1)]

         [(and (eq? state 1) (eq? type 'symbol))
          (set! state -1)]

         [(and (> state 0) (eq? type 'parenthesis))
          (set! state (if (is-open paren)
                          (add1 state)
                          (sub1 state)))
          (when (eq? state 1)
            (set! state -1))]

         [(eq? state -1)
          (set! state 0)])

       (if (eq? state 0)
           tok
           (token lexeme 'comment paren start end mode diff))]
      [tok tok]))

  (define (new-next-token)
    (reclassify (next-token)))
  new-next-token)

(module+ test
  (check-token-transform
   sexp-comment-reclassifier
   (list
    (token #f 'sexp-comment #f #f #f #f #f)
    (token #f 'symbol #f #f #f #f #f)
    (token #f 'symbol #f #f #f #f #f)
    eof-token)
   (list
    (token #f 'comment #f #f #f #f #f)
    (token #f 'comment #f #f #f #f #f)
    (token #f 'symbol #f #f #f #f #f)
    eof-token))

  (check-token-transform
   sexp-comment-reclassifier
   (list
    (token #f 'sexp-comment #f #f #f #f #f)
    (token #f 'parenthesis '|(| #f #f #f #f)
    (token #f 'symbol #f #f #f #f #f)
    (token #f 'parenthesis '|)| #f #f #f #f)
    (token #f 'symbol #f #f #f #f #f)
    eof-token)
   (list
    (token #f 'comment #f #f #f #f #f)
    (token #f 'comment '|(| #f #f #f #f)
    (token #f 'comment #f #f #f #f #f)
    (token #f 'comment '|)| #f #f #f #f)
    (token #f 'symbol #f #f #f #f #f)
    eof-token)))

;; Reclassifies tokens based on semantic information
(define ((semantic-reclassifier intervals errors) next-token)
  (define delta 0)
  (define error-imap (make-interval-map))
  (for-each
   (lambda (e)
     (match-define (exception _ _ srclocs) e)
     (for-each
      (lambda (sl)
        (match-define (srcloc _ _ _ start span) sl)
        (interval-map-set! error-imap start (+ start span) span))
      srclocs))
   errors)

  (define (new-next-token)
    (match (next-token)
      [(token lexeme (? (curry eq? 'symbol) type) data start end
              (? (lambda (m) (or (eq? m 'racket) (eq? m 'scribble))) mode)
              diff)
       (define pos (+ start (floor (/ (- end start) 2))))
       (match-define-values (err-start err-end err-span)
                            (interval-map-ref/bounds error-imap start #f))
       (match-define-values (sem-start sem-end new-data)
                            (interval-map-ref/bounds intervals start #f))
       (when sem-end
         (set! delta (- sem-end end)))
       (when err-span
           (set! delta (+ delta err-span)))
       (token lexeme type new-data start end mode diff)]
    [token token]))
  new-next-token)

(module+ test
  (require rackunit)

  (define racket-str #<<RACKET
#lang racket/base
(define r 10)
(define pi 3.14)
(displayln (* 2 pi r))
RACKET
)

(check-equal? (take (apply-tokenizer-maker make-tokenizer racket-str) 3)
              (list
               (token "#lang racket/base" 'other #f 1 18 'racket #f)
               (token "\n" 'white-space #f 18 19 'racket #f)
               (token "(" 'parenthesis '|(| 19 20 'racket #f)))

(define scribble-str #<<SCRIBBLE
#lang scribble/manual
@(require (for-label json))

@title{jsonic: because JSON is boring}
@author{Roxy Lexington}

@defmodulelang[jsonic]

@section{Introduction}

This is a domain-specific language
that relies on the @racketmodname[json] library.

In particular, the @racket[jsexpr->string] function.

If we start with this:

@verbatim|{
#lang jsonic
[
 @$ 'null $@,
 @$ (* 6 7) $@,
 @$ (= 2 (+ 1 1)) $@
 ]
}|

We'll end up with this:

@verbatim{
[
 null,
 42,
 true
 ]
}
SCRIBBLE
  )

(check-equal? (take (apply-tokenizer-maker make-tokenizer scribble-str) 3)
              (list
               (token "#lang scribble/manual" 'other #f 1 22 'scribble #f)
               (token " " 'white-space #f 22 23 'scribble #f)
               (token "@" 'parenthesis #f 23 24 'scribble #f)))

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
  )

(check-equal? (take (apply-tokenizer-maker make-tokenizer electron-str) 3)
              (list
               (token "#lang electron" 'other #f 1 15 'other #f)
               (token "\n" 'no-color #f 15 16 'other #f)
               (token "// comment line" 'comment #f 16 31 'other #f)))
)


(provide apply-tokenizer-maker make-tokenizer
         token eof-token eof-token? list->producer
         skip-white lang-tokenizer
         sexp-comment-reclassifier semantic-reclassifier)
