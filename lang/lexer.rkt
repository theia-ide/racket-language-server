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
  (require rackunit))

;; Definitions
(struct token (lexeme type data start end mode diff) #:transparent)

(define eof-token (token eof #f #f #f #f #f #f))

(define (eof-token? tok)
  (match tok
    [(token (? eof-object?) _ _ _ _ _ _) #t]
    [_ #f]))


;; Lexer
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

;; Helpers
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
      [(? pair?) (car mode)]
      ['no-lang-line 'no-lang-line]))
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


;; Token stream transformers
(module+ test
  (define (check-token-transform transform tokens-in tokens-out)
    (define next-token (transform (list->producer tokens-in)))
    (check-equal?
     (append
      (for/list ([tok (in-producer next-token eof-token?)])
        tok)
      (list eof-token))
     tokens-out)))

;; Lang tokenizer
;;
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

;; Skip whitespace
;;
;; Skips white space tokens in token stream.
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

;; Sexp comment reclassifier
;;
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

;; Token stream matcher
;;
;; Matches a token stream to an old list of tokens and
;; sets the offset into the old token stream.
(define ((token-stream-matcher old-tokens) next-token)
  (define old-next-token (list->producer old-tokens))
  (define offset 0)

  (define (offset-delta tok old-tok)
    (match-define (token lexeme type _ start end _ _) tok)
    (match old-tok
      [(token olexeme otype _ ostart oend _ _)
       (cond
         ;; old token
         [(eq? type otype) (- oend end)]
         ;; new token
         [else #f])]
      [_ #f]))

  (define new-next-token
    (infinite-generator
     (define old-tok (old-next-token))
     (define (loop)
       (define tok (next-token))
       (match-define (token lexeme type data start end mode _) tok)

       (define offset (offset-delta tok old-tok))
       (yield (token lexeme type data start end mode offset))
       (when (not offset) (loop)))
     (loop)))

  new-next-token)

(module+ test
  (check-token-transform
   (token-stream-matcher '())
   (list
    (token #f 'a #f 1 2 #f #f)
    (token #f 'b #f 2 3 #f #f)
    eof-token)
   (list
    (token #f 'a #f 1 2 #f #f) ; new
    (token #f 'b #f 2 3 #f #f) ; new
    eof-token))

  (check-token-transform
   (token-stream-matcher (list (token "a" 'a #f 1 2 #f #f)
                               (token "b" 'b #f 2 3 #f #f)
                               (token "c" 'c #f 3 4 #f #f)))
   (list
    (token "a" 'a #f 1 2 #f #f) ; no-change
    (token "a" 'a #f 2 3 #f #f) ; new
    (token "B" 'b #f 3 4 #f #f) ; change
    (token "c" 'c #f 4 5 #f #f) ; no-change
    eof-token)
   (list
    (token "a" 'a #f 1 2 #f 0)
    (token "a" 'a #f 2 3 #f #f)
    (token "B" 'b #f 3 4 #f -1)
    (token "c" 'c #f 4 5 #f -1)
    eof-token))

  ;; to simplify implementation removal of old tokens is handled
  ;; in the workspace
  #;(check-token-transform
   (token-stream-matcher (list (token "a" 'a #f 1 2 #f #f)
                               (token "b" 'b #f 2 3 #f #f)
                               (token "c" 'c #f 3 4 #f #f)))
   (list
    (token "a" 'a #f 1 2 #f #f) ; no-change ; deleted
    (token "c" 'c #f 2 3 #f #f) ; no-change
    eof-token)
   (list
    (token "a" 'a #f 1 2 #f 0)
    (token "c" 'c #f 2 3 #f -1)
    eof-token)))

;; Semantic token reclassifier
;;
;; Reclassifies tokens based on semantic information.
(define ((semantic-reclassifier intervals [old-intervals #f]) next-token)
  (define (new-next-token)
    (match (next-token)
      [(token lexeme (? (curry eq? 'symbol) type) data start end
              (? (lambda (m) (or (eq? m 'racket) (eq? m 'scribble))) mode)
              offset)
       (define pos (if old-intervals
                       (+ start (or offset 0))
                       start))
       (match-define-values
        (sem-start sem-end new-data)
        (interval-map-ref/bounds intervals pos #f))
       (token lexeme type (if new-data new-data data) start end mode offset)]
      [token token]))
  new-next-token)

(provide apply-tokenizer-maker make-tokenizer
         token eof-token eof-token? list->producer
         skip-white lang-tokenizer token-stream-matcher
         sexp-comment-reclassifier semantic-reclassifier)
