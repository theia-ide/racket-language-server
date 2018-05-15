#lang racket/base
(require syntax-color/module-lexer
         racket/match
         rackunit)

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
        (list lexeme type data start end)))
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

(check-equal? (apply-tokenizer-maker make-tokenizer racket-str)
              '(("#lang racket/base" other #f 1 18)
                ("\n" white-space #f 18 19)
                ("(" parenthesis |(| 19 20)
                ("define" symbol #f 20 26)
                (" " white-space #f 26 27)
                ("r" symbol #f 27 28)
                (" " white-space #f 28 29)
                ("10" constant #f 29 31)
                (")" parenthesis |)| 31 32)
                ("\n" white-space #f 32 33)
                ("(" parenthesis |(| 33 34)
                ("define" symbol #f 34 40)
                (" " white-space #f 40 41)
                ("pi" symbol #f 41 43)
                (" " white-space #f 43 44)
                ("3.14" constant #f 44 48)
                (")" parenthesis |)| 48 49)
                ("\n" white-space #f 49 50)
                ("(" parenthesis |(| 50 51)
                ("displayln" symbol #f 51 60)
                (" " white-space #f 60 61)
                ("(" parenthesis |(| 61 62)
                ("*" symbol #f 62 63)
                (" " white-space #f 63 64)
                ("2" constant #f 64 65)
                (" " white-space #f 65 66)
                ("pi" symbol #f 66 68)
                (" " white-space #f 68 69)
                ("r" symbol #f 69 70)
                (")" parenthesis |)| 70 71)
                (")" parenthesis |)| 71 72)))

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
              '(("#lang scribble/manual" other #f 1 22)
                (" " white-space #f 22 23)
                ("@" parenthesis #f 23 24)
                ("(" parenthesis |(| 24 25)
                ("require" symbol #f 25 32)
                (" " white-space #f 32 33)
                ("(" parenthesis |(| 33 34)
                ("for-label" symbol #f 34 43)
                (" " white-space #f 43 44)
                ("json" symbol #f 44 48)
                (")" parenthesis |)| 48 49)
                (")" parenthesis |)| 49 50)
                (" " white-space #f 50 52)
                ("@" parenthesis #f 52 53)
                ("title" symbol #f 53 58)
                ("{" parenthesis |{| 58 59)
                (text text #f 59 89)
                ("}" parenthesis |}| 89 90)
                (" " white-space #f 90 91)
                ("@" parenthesis #f 91 92)
                ("author" symbol #f 92 98)
                ("{" parenthesis |{| 98 99)
                (text text #f 99 113)
                ("}" parenthesis |}| 113 114)
                (" " white-space #f 114 116)
                ("@" parenthesis #f 116 117)
                ("defmodulelang" symbol #f 117 130)
                ("[" parenthesis |[| 130 131)
                ("jsonic" symbol #f 131 137)
                ("]" parenthesis |]| 137 138)
                (" " white-space #f 138 140)
                ("@" parenthesis #f 140 141)
                ("section" symbol #f 141 148)
                ("{" parenthesis |{| 148 149)
                (text text #f 149 161)
                ("}" parenthesis |}| 161 162)
                (" " white-space #f 162 164)
                (text text #f 164 198)
                (" " white-space #f 198 199)
                (text text #f 199 218)
                ("@" parenthesis #f 218 219)
                ("racketmodname" symbol #f 219 232)
                ("[" parenthesis |[| 232 233)
                ("json" symbol #f 233 237)
                ("]" parenthesis |]| 237 238)
                (text text #f 238 247)
                (" " white-space #f 247 249)
                (text text #f 249 268)
                ("@" parenthesis #f 268 269)
                ("racket" symbol #f 269 275)
                ("[" parenthesis |[| 275 276)
                ("jsexpr->string" symbol #f 276 290)
                ("]" parenthesis |]| 290 291)
                (text text #f 291 301)
                (" " white-space #f 301 303)
                (text text #f 303 325)
                (" " white-space #f 325 327)
                ("@" parenthesis #f 327 328)
                ("verbatim" symbol #f 328 336)
                (#"" parenthesis |{| 336 338)
                (" " white-space #f 338 339)
                (text text #f 339 351)
                (" " white-space #f 351 352)
                (text text #f 352 353)
                (" " white-space #f 353 355)
                (text text #f 355 367)
                (" " white-space #f 367 369)
                (text text #f 369 383)
                (" " white-space #f 383 385)
                (text text #f 385 404)
                (" " white-space #f 404 406)
                (text text #f 406 407)
                (" " white-space #f 407 408)
                ("}" parenthesis |}| 408 410)
                (" " white-space #f 410 412)
                (text text #f 412 435)
                (" " white-space #f 435 437)
                ("@" parenthesis #f 437 438)
                ("verbatim" symbol #f 438 446)
                ("{" parenthesis |{| 446 447)
                (" " white-space #f 447 448)
                (text text #f 448 449)
                (" " white-space #f 449 451)
                (text text #f 451 456)
                (" " white-space #f 456 458)
                (text text #f 458 461)
                (" " white-space #f 461 463)
                (text text #f 463 467)
                (" " white-space #f 467 469)
                (text text #f 469 470)
                (" " white-space #f 470 471)
                ("}" parenthesis |}| 471 472)))

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
              '(("#lang electron" other #f 1 15)
                ("\n" no-color #f 15 16)
                ("// comment line" comment #f 16 31)
                ("\n" no-color #f 31 32)
                ("/* multiline comment */" comment #f 32 55)
                ("\n" no-color #f 55 56)
                ("module" keyword #f 56 62)
                (" " no-color #f 62 63)
                ("MOD" symbol #f 63 66)
                (" " no-color #f 66 67)
                ("{" parenthesis |(| 67 68)
                ("\n" no-color #f 68 69)
                (" " no-color #f 69 70)
                (" " no-color #f 70 71)
                ("port" keyword #f 71 75)
                (" " no-color #f 75 76)
                ("PORT" symbol #f 76 80)
                ("\n" no-color #f 80 81)
                (" " no-color #f 81 82)
                (" " no-color #f 82 83)
                ("net" keyword #f 83 86)
                (" " no-color #f 86 87)
                ("NET" symbol #f 87 90)
                ("\n" no-color #f 90 91)
                (" " no-color #f 91 92)
                (" " no-color #f 92 93)
                ("OTHERMOD" symbol #f 93 101)
                (" " no-color #f 101 102)
                ("{" parenthesis |(| 102 103)
                ("PORT" symbol #f 103 107)
                ("," no-color #f 107 108)
                (" " no-color #f 108 109)
                ("OTHERPORT" symbol #f 109 118)
                ("=" no-color #f 118 119)
                ("NET" symbol #f 119 122)
                ("}" parenthesis |)| 122 123)
                ("\n" no-color #f 123 124)
                ("}" parenthesis |)| 124 125)))
)


(provide make-tokenizer)
