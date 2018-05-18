#lang racket/base
(require syntax-color/module-lexer
         syntax-color/racket-lexer
         syntax-color/scribble-lexer
         racket/match
         racket/list
         rackunit)

(define (mode->symbol mode)
  (define mode-proc
    (match mode
      [(? procedure?) mode]
      [(? pair?) (car mode)]))
  (cond
    [(equal? mode-proc racket-lexer) 'racket]
    [(equal? mode-proc scribble-inside-lexer) 'scribble]
    [else 'other]))

;; Splits 'other tokens starting with #lang into a 'lang-keyword
;; and 'lang-symbol token.
(define (lang-tokenizer next-token)
  (define lang-tokens #f)
  (define (tokenize-lang text start end)
    (define lang-line
      (regexp-match #px"^(#lang)(\\s+)(.+)$" text))
    (match lang-line
      [(list all lang white symbol)
       (define end-lang (+ start 5))
       (define end-white (+ start 6 (string-length white)))
       (list
        (list lang 'keyword #f start end-lang 'lang)
        (list white 'white-space #f (add1 end-lang) end-white 'lang)
        (list symbol 'symbol #f (add1 end-white) end 'lang))]
      [_ #f]))

  (define (lang-next-token)
    (define token (car lang-tokens))
    (set! lang-tokens (cdr lang-tokens))
    (when (empty? lang-tokens)
      (set! lang-tokens #f))
    token)

  (define (default-next-token)
    (define token (next-token))
    (match token
      [(? eof-object?) token]
      [(list lexeme type _ start end _)
       (if (eq? type 'other)
           (begin
             (set! lang-tokens (tokenize-lang lexeme start end))
             (if lang-tokens (lang-next-token) token))
           token)]))

  (define (new-next-token)
    (if lang-tokens (lang-next-token) (default-next-token)))
  new-next-token)

(define (skip-white next-token)
  (define (skip token)
    (match-define (list _ type _ _ _ _) token)
    (if (or (eq? type 'white-space)
            (eq? type 'whitespace)
            (eq? type 'no-color))
        (new-next-token) token))

  (define (new-next-token)
    (define token (next-token))
    (if (eof-object? token) token (skip token)))
  new-next-token)

;; Counts parenthesis and reclassifies the tokens of the sexp after
;; 'sexp-comment to 'comment.
(define (sexp-comment-reclassifier next-token)
  (define (is-open paren)
    (or (eq? paren '|(|) (eq? paren '|{|) (eq? paren '|[|)))
  (define state 0)
  (define (reclassify token)
    (match-define (list lexeme type paren start end mode) token)
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
        token
        (list lexeme 'comment paren start end mode)))

  (define (new-next-token)
    (define token (next-token))
    (if (eof-object? token) token (reclassify token)))
  new-next-token)

(define (get-lexer in)
  (define offset 0)
  (define mode #f)
  (define (next-token)
    (match-define-values
     (lexeme type data start end newOffset newMode)
     (module-lexer in offset mode))
    (set! offset newOffset)
    (set! mode newMode)

    (if (eof-object? lexeme)
        eof
        (list lexeme type data start end (mode->symbol mode))))
  next-token)

(define (make-tokenizer str)
  (define input-port (open-input-string str))
  (port-count-lines! input-port)
  (get-lexer input-port))

(define (apply-tokenizer-maker tokenizer val)
  (define next-token (tokenizer val))
  (for/list ([tok (in-producer next-token eof-object?)])
    tok))

(module+ test
  (define racket-str #<<RACKET
#lang racket/base
(define r 10)
(define pi 3.14)
(displayln (* 2 pi r))
RACKET
)

(check-equal? (apply-tokenizer-maker
               (compose lang-tokenizer make-tokenizer)
               racket-str)
              '(("#lang" keyword #f 1 6 lang)
                (" " white-space #f 7 8 lang)
                ("racket/base" symbol #f 9 18 lang)
                ("\n" white-space #f 18 19 racket)
                ("(" parenthesis |(| 19 20 racket)
                ("define" symbol #f 20 26 racket)
                (" " white-space #f 26 27 racket)
                ("r" symbol #f 27 28 racket)
                (" " white-space #f 28 29 racket)
                ("10" constant #f 29 31 racket)
                (")" parenthesis |)| 31 32 racket)
                ("\n" white-space #f 32 33 racket)
                ("(" parenthesis |(| 33 34 racket)
                ("define" symbol #f 34 40 racket)
                (" " white-space #f 40 41 racket)
                ("pi" symbol #f 41 43 racket)
                (" " white-space #f 43 44 racket)
                ("3.14" constant #f 44 48 racket)
                (")" parenthesis |)| 48 49 racket)
                ("\n" white-space #f 49 50 racket)
                ("(" parenthesis |(| 50 51 racket)
                ("displayln" symbol #f 51 60 racket)
                (" " white-space #f 60 61 racket)
                ("(" parenthesis |(| 61 62 racket)
                ("*" symbol #f 62 63 racket)
                (" " white-space #f 63 64 racket)
                ("2" constant #f 64 65 racket)
                (" " white-space #f 65 66 racket)
                ("pi" symbol #f 66 68 racket)
                (" " white-space #f 68 69 racket)
                ("r" symbol #f 69 70 racket)
                (")" parenthesis |)| 70 71 racket)
                (")" parenthesis |)| 71 72 racket)))

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

(check-equal? (apply-tokenizer-maker make-tokenizer scribble-str)
              '(("#lang scribble/manual" other #f 1 22 scribble)
                (" " white-space #f 22 23 scribble)
                ("@" parenthesis #f 23 24 scribble)
                ("(" parenthesis |(| 24 25 scribble)
                ("require" symbol #f 25 32 scribble)
                (" " white-space #f 32 33 scribble)
                ("(" parenthesis |(| 33 34 scribble)
                ("for-label" symbol #f 34 43 scribble)
                (" " white-space #f 43 44 scribble)
                ("json" symbol #f 44 48 scribble)
                (")" parenthesis |)| 48 49 scribble)
                (")" parenthesis |)| 49 50 scribble)
                (" " white-space #f 50 52 scribble)
                ("@" parenthesis #f 52 53 scribble)
                ("title" symbol #f 53 58 scribble)
                ("{" parenthesis |{| 58 59 scribble)
                (text text #f 59 89 scribble)
                ("}" parenthesis |}| 89 90 scribble)
                (" " white-space #f 90 91 scribble)
                ("@" parenthesis #f 91 92 scribble)
                ("author" symbol #f 92 98 scribble)
                ("{" parenthesis |{| 98 99 scribble)
                (text text #f 99 113 scribble)
                ("}" parenthesis |}| 113 114 scribble)
                (" " white-space #f 114 116 scribble)
                ("@" parenthesis #f 116 117 scribble)
                ("defmodulelang" symbol #f 117 130 scribble)
                ("[" parenthesis |[| 130 131 scribble)
                ("jsonic" symbol #f 131 137 scribble)
                ("]" parenthesis |]| 137 138 scribble)
                (" " white-space #f 138 140 scribble)
                ("@" parenthesis #f 140 141 scribble)
                ("section" symbol #f 141 148 scribble)
                ("{" parenthesis |{| 148 149 scribble)
                (text text #f 149 161 scribble)
                ("}" parenthesis |}| 161 162 scribble)
                (" " white-space #f 162 164 scribble)
                (text text #f 164 198 scribble)
                (" " white-space #f 198 199 scribble)
                (text text #f 199 218 scribble)
                ("@" parenthesis #f 218 219 scribble)
                ("racketmodname" symbol #f 219 232 scribble)
                ("[" parenthesis |[| 232 233 scribble)
                ("json" symbol #f 233 237 scribble)
                ("]" parenthesis |]| 237 238 scribble)
                (text text #f 238 247 scribble)
                (" " white-space #f 247 249 scribble)
                (text text #f 249 268 scribble)
                ("@" parenthesis #f 268 269 scribble)
                ("racket" symbol #f 269 275 scribble)
                ("[" parenthesis |[| 275 276 scribble)
                ("jsexpr->string" symbol #f 276 290 scribble)
                ("]" parenthesis |]| 290 291 scribble)
                (text text #f 291 301 scribble)
                (" " white-space #f 301 303 scribble)
                (text text #f 303 325 scribble)
                (" " white-space #f 325 327 scribble)
                ("@" parenthesis #f 327 328 scribble)
                ("verbatim" symbol #f 328 336 scribble)
                (#"" parenthesis |{| 336 338 scribble)
                (" " white-space #f 338 339 scribble)
                (text text #f 339 351 scribble)
                (" " white-space #f 351 352 scribble)
                (text text #f 352 353 scribble)
                (" " white-space #f 353 355 scribble)
                (text text #f 355 367 scribble)
                (" " white-space #f 367 369 scribble)
                (text text #f 369 383 scribble)
                (" " white-space #f 383 385 scribble)
                (text text #f 385 404 scribble)
                (" " white-space #f 404 406 scribble)
                (text text #f 406 407 scribble)
                (" " white-space #f 407 408 scribble)
                ("}" parenthesis |}| 408 410 scribble)
                (" " white-space #f 410 412 scribble)
                (text text #f 412 435 scribble)
                (" " white-space #f 435 437 scribble)
                ("@" parenthesis #f 437 438 scribble)
                ("verbatim" symbol #f 438 446 scribble)
                ("{" parenthesis |{| 446 447 scribble)
                (" " white-space #f 447 448 scribble)
                (text text #f 448 449 scribble)
                (" " white-space #f 449 451 scribble)
                (text text #f 451 456 scribble)
                (" " white-space #f 456 458 scribble)
                (text text #f 458 461 scribble)
                (" " white-space #f 461 463 scribble)
                (text text #f 463 467 scribble)
                (" " white-space #f 467 469 scribble)
                (text text #f 469 470 scribble)
                (" " white-space #f 470 471 scribble)
                ("}" parenthesis |}| 471 472 scribble)))

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

(check-equal? (apply-tokenizer-maker make-tokenizer electron-str)
              '(("#lang electron" other #f 1 15 other)
                ("\n" no-color #f 15 16 other)
                ("// comment line" comment #f 16 31 other)
                ("\n" no-color #f 31 32 other)
                ("/* multiline comment */" comment #f 32 55 other)
                ("\n" no-color #f 55 56 other)
                ("module" keyword #f 56 62 other)
                (" " no-color #f 62 63 other)
                ("MOD" symbol #f 63 66 other)
                (" " no-color #f 66 67 other)
                ("{" parenthesis |(| 67 68 other)
                ("\n" no-color #f 68 69 other)
                (" " no-color #f 69 70 other)
                (" " no-color #f 70 71 other)
                ("port" keyword #f 71 75 other)
                (" " no-color #f 75 76 other)
                ("PORT" symbol #f 76 80 other)
                ("\n" no-color #f 80 81 other)
                (" " no-color #f 81 82 other)
                (" " no-color #f 82 83 other)
                ("net" keyword #f 83 86 other)
                (" " no-color #f 86 87 other)
                ("NET" symbol #f 87 90 other)
                ("\n" no-color #f 90 91 other)
                (" " no-color #f 91 92 other)
                (" " no-color #f 92 93 other)
                ("OTHERMOD" symbol #f 93 101 other)
                (" " no-color #f 101 102 other)
                ("{" parenthesis |(| 102 103 other)
                ("PORT" symbol #f 103 107 other)
                ("," no-color #f 107 108 other)
                (" " no-color #f 108 109 other)
                ("OTHERPORT" symbol #f 109 118 other)
                ("=" no-color #f 118 119 other)
                ("NET" symbol #f 119 122 other)
                ("}" parenthesis |)| 122 123 other)
                ("\n" no-color #f 123 124 other)
                ("}" parenthesis |)| 124 125 other)))
)


(provide make-tokenizer skip-white sexp-comment-reclassifier lang-tokenizer)
