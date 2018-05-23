# Racket Language Server

Implementation of the
[Language Server Protocol](https://microsoft.github.io/language-server-protocol/specification)
for [Racket](https://racket-lang.org/).

## Custom extensions
#### <a name="racket_colorize" class="anchor"></a>Colorize Notification (:arrow_left:)

Colorize notifications are sent from the server to the client to enable semantic
syntax highlighting.

When a file changes it is the server's responsibility to re-compute syntax
highlighting and push them to the client. Newly pushed tokens always replace
previously pushed tokens. There is no merging that happens on the client side.

_Notification_:
* method: 'racket/colorize'
* params: `RacketColorizeParams` defined as follows:

```typescript
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
```

#### <a name="racket_indent" class="anchor"></a>Indent Request (:arrow_left:)

Indent requests are sent from the client to the server to compute the
indentation for a line number.

_Request_:
* method: 'racket/indent'
* params: `RacketIndentParams` defined as follows:

```typescript
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
```

_Response_:
* result: `number`
* error: code and message set in case an exception happens during the definition request.

## References
- [0] https://microsoft.github.io/language-server-protocol/specification
- [1] https://docs.racket-lang.org/tools/lang-languages-customization.html
- [2] https://github.com/jeapostrophe/racket-langserver

## License
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
