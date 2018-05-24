#lang scribble/manual
@(require scribble/extract
          (for-label racket))

@title{Racket Language Server}
@author{David Craven}

@defmodule[racket-language-server]

Implementation of the
@hyperlink["https://microsoft.github.io/language-server-protocol/specification"]{Language Server Protocol}
for @hyperlink["https://racket-lang.org/"]{Racket}.

@table-of-contents[]

@section[#:tag "custom-extensions"]{Custom Extensions}

@subsection[#:tag "racket_colorize"]{Colorize Notification}

Colorize notifications are sent from the server to the client to enable semantic
syntax highlighting.

When a file changes it is the server's responsibility to re-compute syntax
highlighting and push them to the client. Newly pushed tokens always replace
previously pushed tokens. There is no merging that happens on the client side.

@italic{Notification}:
@itemize{
  @item{method: 'racket/colorize'}
  @item{params: `RacketColorizeParams` defined as follows:}
}

@verbatim|{
interface RacketColorizeParams {
    /**
     * The URI for which colorization is reported.
     */
    uri: DocumentUri;

    /**
     * An array of diagnostic information items.
     */
    tokens: RacketToken[];
}

interface RacketToken {
    /**
     * The kind of token.
     */
    kind: RacketTokenKind;

    /**
     * The lexer mode used.
     */
    mode: RacketLexerMode;

    /**
     * Range of the token.
     */
    range: Range;
}

enum RacketLexerMode {
    Racket = "racket",
    Scribble = "scribble",
    Other = "other"
}

enum RacketTokenKind {
    // Generic token kinds
    Symbol = "symbol",
    Keyword = "keyword",
    Comment = "comment",
    String = "string",
    Constant = "constant",
    Parenthesis = "parenthesis",
    Error = "error",
    NoColor = "no-color",
    Other = "other",
    // Racket token kinds
    HashColonKeyword = "hash-colon-keyword",
    SexpComment = "sexp-comment",
    // Racket semantic token kinds
    Imported = "imported",
    LexicallyBound = "lexically-bound",
    Free = "free",
    Set!d = "set!d",
    // Scribble token kinds
    Text = "text"
}
}|

@subsection[#:tag "racket_indent"]{Indent Request}

Indent requests are sent from the client to the server to compute the
indentation for a line number.

@italic{Request}:
@itemize{
  @item{method: 'racket/indent'}
  @item{params: `RacketIndentParams` defined as follows:}
}

@verbatim|{
interface RacketIndentParams {
    /**
     * The TextDocument for which indentation is requested.
     */
    textDocument: TextDocumentIdentifier;

    /**
     * The line to indent.
     */
    line: number;
}
}|

@italic{Response}:
@itemize{
  @item{result: `number`}
  @item{error: code and message set in case an exception happens during the
  definition request.}
}

@section[#:tag "api"]{Application Programming Interface}
The codebase is split in two folders, `lang` which provides Racket language
support procedures that extract and combine information from drracket and other
sources and `protocol` which converts the information to the Language Server
Protocol.

@subsection[#:tag "lang"]{Language support}
@declare-exporting[racket-language-server/lang/lexer racket-language-server]
@include-extracted["../lang/lexer.rkt"]

@subsection[#:tag "protocol"]{Protocol}

@section[#:tag "references"]{References}

@itemize{
  @item{[0] @url{https://microsoft.github.io/language-server-protocol/specification}}
  @item{[1] @url{https://docs.racket-lang.org/tools/lang-languages-customization.html}}
  @item{[2] @url{https://github.com/jeapostrophe/racket-langserver}}
}

@section[#:tag "license"]{License}
Copyright 2018 David Craven and others

Permission to use, copy, modify, and/or distribute this software for any purpose
with or without fee is hereby granted, provided that the above copyright notice
and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF
THIS SOFTWARE.