# Scribble: SCRibe-like reader extension for Common Lisp

Copyright (c) 2002-2023 by Fare Rideau < fare at tunes dot org >
< http://www.cliki.net/Fare%20Rideau >

## Home Page
< http://www.cliki.net/Scribble >

## License
I generally use the Bugroff License:
< http://tunes.org/legalese/bugroff.html >

You may at your leisure use the LLGPL instead:
< http://www.cliki.net/LLGPL >

## Dependency
This package depends on Meta by Jochen Schmidt, version 1.0.0 or later.
< http://www.cliki.net/Meta >
Now also: meta fare-utils fare-matcher named-readtables

## Usage
You can enable Racket-like Scribble behavior for the macro-character `#\@` with:
```Lisp
(scribble:enable-scribble-at-syntax)
```
And you can disable it with:
```Lisp
(scribble:disable-scribble-at-syntax)
```
You may also use:
```Lisp
(named-readtables:in-readtable :scribble-racket)
```
Or:
```Lisp
(named-readtables:in-readtable :scribble-both)
```
Or:
```Lisp
(named-readtables:in-readtable :scribble)
```
For details, see:
< http://docs.racket-lang.org/scribble/reader.html >

If you additionally pass the keyword argument `:skribe t`
You will also have Skribe-like syntax.

You can enable only Skribe-like syntax for the macro-character `#\[` with:
```Lisp
(scribble:enable-scribble-syntax)
```
and disable it with:
```Lisp
(scribble:disable-scribble-syntax)
```
You may also use:
```Lisp
(named-readtables:in-readtable :scribble-skribe)
```
Or:
```Lisp
(named-readtables:in-readtable :scribble-both)
```
Or
```Lisp
(named-readtables:in-readtable :scribble)
```

Alternatively, you can enable behaviour for the character `#\[`
under the dispatching macro-character `#\#` using:
```Lisp
(scribble:enable-sub-scribble-syntax)
(scribble:disable-sub-scribble-syntax)
```

## At-syntax
The syntax of text after `@` is just like
Racket's Scribble syntax by Eli Barzilay.
His Scribble "at-syntax" is described thus:
    < http://barzilay.org/research.html >
    < http://barzilay.org/misc/scribble-reader.pdf >
    < http://docs.racket-lang.org/scribble/reader.html >

## Basic Syntax
The syntax of text within brackets `[...]` is Skribe-like:

* Text between brackets will expand to a string containing said text,
  unless there are escape forms in the text,
  which are identified by a comma `#\,`
  followed by either an opening parenthesis `#\(` or an opening bracket `#\[`.

* If there are escape forms in the text,
  then the text will be split into components,
  which will be non-empty strings and escape forms.
  The result of parsing the text will be a `(LIST ...)` of these components.

* A comma `#\,` followed by a parenthesis `#\(` denotes an escape form,
  wherein a SEXP beginning with said parenthesis
  is read and used as a component of the surrounding text.
  For instance, `[foo ,(bar baz) quux]`
  will (on first approximation) be read as `(LIST "foo " (bar baz) " quux")`
  Mind the spaces being preserved around the internal form.

## Extension to Scribe syntax
Scribble extends the Scribe syntax in a way that I find very convenient.

* As an extension to Scribe syntax,
  if the first character of text within bracket is an unescaped colon `#\:`
  then an expression after it is read that is used
  as a "head" for the body of the text resulting from parsing as above.
    `[:emph this]` is read as `(EMPH "this")`, and
    `[:(font :size -1) that]` is read as `(FONT :SIZE -1 "that")`.

* As another extension to Scribe syntax,
  a comma `#\,` followed by a bracket `#\[` will also denote an escape form,
  whereas the bracketed-text using Scribble syntax
  is read and used as a component of the surrounding text.
  This extension is only useful in conjunction with the previous extension.

## Syntactic catches
There are a few possible sources of problems with the Scribe syntax,
and solutions provided by Scribe and Scribble to avoid these problems.

* A closing bracket `#\]` closes the current text.
  Standard Scribe syntax doesn't provide a mean
  to include a closing bracket in bracketed text.

* Conversely, so as to prevent difficult to track syntax errors
  resulting from typos, Standard Scribe syntax forbids
  to include an opening bracket in the text.

* As an extension to Scribe syntax,
  you can include any character in the text,
  without triggering any special or error-raising behaviour,
  by preceding it with a backslash character `#\\` in the text
  (which preceding backslash character won't be included in the result string).
  This is useful to include a character among
  `#\\` `#\:` `#\,` `#\[` `#\]` `#\(`.

* While `#\\` will always be able to escape all non-alphanumeric characters,
  including the special characters listed above,
  future extensions may give a special meaning to `#\\` followed by a character
  in the regexp range `[_a-zA-Z0-9]`.
  If you feel the need for such an extension, I will accept patches;
  I suppose that the C or Perl syntax is what is needed here.

* In the bracket-colon `[:` syntax extension, after reading the "head",
  all spacing characters
  (`#\space`, `#\tab`, `#\newline`, `#\linefeed`, `#\return`, `#\page`)
  are skipped until the next non-space character:
  `[:head     no space!]` is read as `(HEAD "no space!")`.
  To insert a space character immediately after the head,
  just escape it using `#\\` as above:
  `[:head\ <- this space]` is read as `(HEAD " <- this space")`.

* As a restriction from Scribe syntax, Scribble syntax doesn't recognize
  the use of semi-colon `#\;` as denoting discardable comments.
  In Scribe, a semi-colon `#\;` at the beginning of a line or of bracketed text
  or of a string component of bracketed text will denote a comment,
  whereas Scribe will ignore any text to the next end of line.
  Scribble will include any such text in the result string.
  You can emulate the original Scribe behaviour in this regard
  by using the preprocessing customization feature described below.

## Customization
Scribble can be customized in many ways,
to accomodate the specificities of your markup language backend.

* As an extension to Scribe semantics,
  all strings resulting from reading bracket-delimited text `[...]`
  (as opposed to those resulting from "normal" double-quote delimited `"..."`
  strings that may appear inside escape forms) may be preprocessed.
  There can be compile-time or run-time preprocessing.
  The variable `*scribble-preprocess*` decides
  what kind of preprocessing is done.
  If it is `nil`, then no preprocessing is done
  (i.e. strings from the `[...]` notation will be read as such).
  If it is `t`, then run-time preprocessing is done,
  via the function `pp` which itself issues
  a dynamic call to the function `*scribble-preprocessor*`
  if not `nil` (or else behaves as `identity`).
  If it is a function or non-boolean symbol, then said value
  is `funcall`'ed at read-time to preprocess the string form
  by e.g. wrapping it into some macro evaluation.
  Note that when using run-time preprocessing,
  you may either lexically shadow the function `pp` or
  dynamically rebind the variable `*scribble-preprocessor*`
  to locally select a different preprocessor.
  A macro `scribble:with-preprocessor` is defined to do the dynamic rebinding,
  as in `(scribble:with-preprocessor #'string-upcase [foo])` which
  (assuming run-time preprocessing is enabled) will evaluate to `"FOO"`.

* Though the default behaviour of Scribble is to return
  a (possibly preprocessed) string if there are no subcomponents,
  and a form `(cl:list ...)` if there are multiple components,
  you can customize this behaviour by binding the customization variable
  `scribble:*scribble-list*` to a function that will do the job,
  taking as many arguments as there were components (zero for empty text).
  If you only want to keep the same general behaviour,
  but change the head of the resulting list from `cl:list` to something else,
  then don't modify `scribble:*scribble-list*`
  (or bind it back to `scribble:default-scribble-list`)
  and instead bind `scribble:*scribble-default-head*` to a symbol
  that at evaluation time will be bound to a function
  that will properly combine the multiple components.
  Note that this `scribble:*scribble-list*` is processed at read-time,
  whereas the function named by `scribble:*scribble-default-head*`
  (if applicable) will be processed at evaluation-time.

* You can select a package from which Scribble will read head forms
  of bracket-colon syntax `[:head ...]` or `[:(head args) ...]`
  by changing the symbol-value of `scribble:*scribble-package*`.
  Typical use is `(setq scribbe:*scribble-package* :keyword)`
  which will do wonders with AllegroServe's `net.html.generator`.
  Note that this feature happens at read-time, and doesn't affect
  the current package used to read escape forms.
  If the `*scribble-package*` feature prevents reading
  the arguments to structured head form arguments in the right package,
  `[:(head form arguments) ...]`
  then you can fall back to normal scribe syntax
  `,(head form argument [...])`
  or qualify the symbols in your head form by their package
  `[:(cl:head my-package:form foo:arguments) ...]`.

* You can modify the way that scribble combines
  the head and body of bracket-colon syntax `[:`
  by changing the value of variable `scribble:*scribble-cons*`
  from the default value `scribble:default-scribble-cons`.
  The function takes as parameters the head specified by bracket-colon syntax
  and the list of components of the bracketed text,
  and has to return the desired parse result.
  Typically, you might want to special case the behaviour
  according to the type of the head: cons or symbol.
  Note that this happens at read-time.

* Example functions to customize scribble for use with various backends
  are given at the end of this file. Check functions
    `scribble:configure-scribble`,
    `scribble:configure-scribble-for-araneida`,
    `scribble:configure-scribble-for-htmlgen`,
    `scribble:configure-scribble-for-lml2`,
    `scribble:configure-scribble-for-tml`,
    `scribble:configure-scribble-for-who`,
    `scribble:configure-scribble-for-yaclml`.
  Please send me updates that include support for your favorite backend.

## Example use
Example use to enable the syntax at the REPL:
```Lisp
(asdf:load-system "scribble")
(use-package :scribble)
(enable-scribble-syntax)
'[foo ,[:emph bar] ,[:(baz :size 1) quux ,(tata toto [\:titi])] tutu]
```
==>
```Lisp
(LIST (PP "foo ") (EMPH (PP "bar")) (PP " ")
 (BAZ :SIZE 1 (LIST (PP "quux ") (TATA TOTO (PP ":titi")))) (PP " tutu"))
```

Example use to read a file after having enabled the syntax:
```Lisp
(let ((p "/home/fare/fare/www/liberty/white_black_magic.scr")
      (eof '#:eof))
  (with-open-file (s p :direction :input :if-does-not-exist :error)
    (loop for i = (read s nil eof nil)
      until (eq i eof)
      collect i)))
```

Example use to configure Scribble to produce Araneida's representation of HTML:
```Lisp
(configure-scribble-for-araneida-html)
(html-stream *stdout* '[:html ...])
```

## TODO
* Make it work with aserve, who, and other backends.

Share and enjoy!

For historical information, see also Daniel Herring's partial implementation:
http://lists.libcl.com/pipermail/libcl-devel-libcl.com/2010-January/000094.html


## Naming Note

Eli Barzilay started using the name "Scribble" in 2006;
I started using it in 2003 or earlier for my Scribe-like syntax, now Skribe-like.
