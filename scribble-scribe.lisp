#+xcvb (module (:depends-on ("package")))

#|" -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
Scribble: SCRibe-like reader extension for Common Lisp
Copyright (c) 2002-2006 by Fare Rideau < fare at tunes dot org >
	http://www.cliki.net/Fare%20Rideau

HOME PAGE:
	http://www.cliki.net/Scribble

LICENSE:
	http://www.geocities.com/SoHo/Cafe/5947/bugroff.html
You may at your leisure use the LLGPL instead:
	http://www.cliki.net/LLGPL

DEPENDENCY:
This package depends on Meta by Jochen Schmidt, version 0.1.1 or later.
	http://www.cliki.net/Meta

USAGE:
You can enable behaviour for the macro-character #\[ with
	(scribble:enable-scribble-syntax)
and disable it with
	(scribble:disable-scribble-syntax)
Alternatively, you can enable behaviour for the character #\[
under the dispatching macro-character #\# using
	(scribble:enable-sub-scribble-syntax)
	(scribble:disable-sub-scribble-syntax)

BASIC SYNTAX:
The syntax of text within brackets is Scribe-like:

* Text between brackets will expand to a string containing said text,
 unless there are escape forms in the text,
 which are identified by a comma
 followed by either an opening parenthesis or an opening bracket.

* If there are escape forms in the text,
 then the text will be split into components,
 which will be non-empty strings and escape forms.
 The result of parsing the text will be a (LIST ...) of these components.

* A comma followed by a parenthesis denotes an escape form,
 whereas a SEXP beginning with said parenthesis
 is read and used as a component of the surrounding text.
 For instance, [foo ,(bar baz) quux] will (on first approximation) be read as
	(LIST "foo " (bar baz) " quux")
 Mind the spaces being preserved around the internal form.

EXTENSION TO SCRIBE SYNTAX:
Scribble extends the Scribe syntax in a way that I find very convenient.

* As an extension to Scribe syntax,
 if the first character of text within bracket is an unescaped colon #\:
 then an expression after it is read that is used
 as a "head" for the body of the text resulting from parsing as above.
	[:emph this] is read as (EMPH "this")
	[:(font :size -1) that] is read as (FONT :SIZE -1 "that")

* As another extension to Scribe syntax,
 a comma followed by a bracket will also denote an escape form,
 whereas the bracketed-text using Scribble syntax
 is read and used as a component of the surrounding text.
 This extension is only useful in conjunction with the previous extension.

SYNTACTIC CATCHES:
There are a few possible sources of problems with the Scribe syntax,
and solutions provided by Scribe and Scribble to avoid these problems.

* A closing bracket closes the current text.
 Standard Scribe syntax doesn't provide a mean
 to include a closing bracket in bracketed text.

* Conversely, so as to prevent difficult to track syntax errors
 resulting from typos, Standard Scribe syntax forbids
 to include an opening bracket in the text.

* As an extension to Scribe syntax,
 you can include any character in the text,
 without triggering any special or error-raising behaviour,
 by preceding it with a backslash character #\\ in the text
 (which preceding backslash character won't be included in the result string).
 This is useful to include a character among #\\ #\: #\, #\[ #\] #\(.

* While #\\ will always be able to escape all non-alphanumeric characters,
 including the special characters listed above,
 future extensions may give a special meaning to #\\ followed by a character
 in the regexp range [_a-zA-Z0-9].
 If you feel the need for such an extension, I will accept patches;
 I suppose that the C or Perl syntax is what is needed here.

* In the bracket-colon syntax extension, after reading the "head",
 all spacing characters (space, tab, newline, linefeed, return, page)
 are skipped until the next non-space character.
 To insert a space character immediately after the head,
 just escape it using #\\ as above.

* As a restriction from Scribe syntax, Scribble syntax
 doesn't recognize the use of semi-colon as denoting discardable comments.
 In Scribe, a semi-colon at the beginning of a line or of bracketed text
 or of a string component of bracketed text will denote a comment,
 whereas Scribe will ignore any text to the next end of line.
 Scribble will include any such text in the result string.
 You can emulate the original Scribe behaviour in this regard
 by using the preprocessing customization feature described below.

CUSTOMIZATION:
Scribble can be customized in many ways,
to accomodate the specificities of your markup language backend.

* As an extension to Scribe semantics, all strings resulting from reading
 bracket-delimited text (as opposed to those resulting from "normal"
 double-quote delimited strings that may appear inside escape forms)
 may be preprocessed. There may be compile-time or run-time preprocessing.
 The variable *SCRIBBLE-PREPROCESS* decides what kind of preprocessing is
 done. If it is NIL, then no preprocessing is done (i.e. strings from the
 [...] notation will be read as such). If it is T, then run-time
 preprocessing is done, via the function PP which itself issues a dynamic
 call to the function *SCRIBBLE-PREPROCESSOR* if not NIL (or else behaves
 as identity). If it is a function or non-boolean symbol, then said value
 is funcall'ed at read-time to preprocess the string form by e.g. wrapping
 it into some macro evaluation. Note that when using run-time preprocessing,
 you may either lexically shadow the function PP or dynamically rebind the
 variable *SCRIBBLE-PREPROCESSOR* to locally select a different preprocessor.
 A macro SCRIBBLE:WITH-PREPROCESSOR is defined to do the dynamic rebinding,
 as in (scribble:with-preprocessor #'string-upcase [foo]) which (assuming
 run-time preprocessing is enabled) will evaluate to "FOO".

* Though the default behaviour of Scribble is to return
 a (possibly preprocessed) string if there are no subcomponents,
 and a form (cl:list ...) if there are multiple components,
 you can customize this behaviour by binding the customization variable
 scribble:*scribble-list* to a function that will do the job,
 taking as many arguments as there were components (zero for empty text).
 If you only want to keep the same general behaviour,
 but change the head of the resulting list from cl:list to something else,
 then don't modify scribble:*scribble-list*
 (or bind it back to scribble:default-scribble-list)
 and instead bind scribble:*scribble-default-head* to a symbol
 that at evaluation time will be bound to a function
 that will properly combine the multiple components.
 Note that this scribble:*scribble-list* is processed at read-time,
 whereas the function named by scribble:*scribble-default-head* (if applicable)
 will be processed at evaluation-time.

* You can select a package from which Scribble will read head forms
 of bracket-colon syntax [:head ...] or [:(head args) ...]
 by changing the symbol-value of scribble:*scribble-package*.
 Typical use is
	(setq scribbe:*scribble-package* :keyword)
 which will do wonders with AllegroServe's net.html.generator.
 Note that this feature happens at read-time, and doesn't affect
 the current package used to read escape forms.
 If the *scribble-package* feature prevents reading
 the arguments to structured head form arguments in the right package,
	[:(head form arguments) ...]
 then you can fall back to normal scribe syntax
	,(head form argument [...])
 or qualify the symbols in your head form by their package
	[:(cl:head my-package:form foo:arguments) ...]

* You can modify the way that scribble combines
 the head and body of bracket-colon syntax
 by changing the value of variable scribble:*scribble-cons*
 from the default value scribble:default-scribble-cons.
 The function takes as parameters the head specified by bracket-colon syntax
 and the list of components of the bracketed text, and has to return
 Typically, you might want to special case the behaviour
 according to the type of the head: cons or symbol.
 Note that this happens at read-time.

* Example functions to customize scribble for use with various backends
 are given at the end of this file. Check functions
	scribble:configure-scribble
	scribble:configure-scribble-for-araneida
	scribble:configure-scribble-for-htmlgen
	scribble:configure-scribble-for-lml2
	scribble:configure-scribble-for-tml
	scribble:configure-scribble-for-who
	scribble:configure-scribble-for-yaclml
 Please send me updates that include support for your favorite backend.

EXAMPLE USE:
(load "scribble")
(use-package :scribble)
(enable-scribble-syntax)
'[foo ,[:emph bar] ,[:(baz :size 1) quux ,(tata toto [\:titi])] tutu]
==>
(LIST (PP "foo ") (EMPH (PP "bar")) (PP " ")
 (BAZ :SIZE 1 (LIST (PP "quux ") (TATA TOTO (PP ":titi")))) (PP " tutu"))

(let ((p "/home/fare/fare/www/liberty/white_black_magic.scr")
      (eof '#:eof))
  (with-open-file (s p :direction :input :if-does-not-exist :error)
    (loop for i = (read s nil eof nil)
      until (eq i eof)
      collect i)))

(configure-scribble-for-araneida-html)
(html-stream *stdout* '[:html ...])

TODO:
* Make it work with aserve, who, and other backends.

Share and enjoy!
" |#

(in-package :scribble)

; -----------------------------------------------------------------------------
;;; Optimization
(declaim (optimize (speed 3) (safety 1) (debug 0)))

; -----------------------------------------------------------------------------
;;; Customizing string preprocessing

(defvar *scribble-preprocess* t
  "set this variable to NIL to disable Scribble wrapping of strings
into preprocessing forms, to T to enable run-time preprocessing, or to a symbol or function
to enable compile-time preprocessing")

(defvar *scribble-preprocessor* nil
  "run-time preprocessor of strings by Scribble. Used when *SCRIBBLE-PREPROCESS* is T.")

(defun pp (x)
  "Default preprocessing of Scribble strings: compile-time identity.
Globally, locally or lexically alter the binding of symbol-function scribble:pp
in your macros so as to customize the behaviour of preprocessing"
  (let ((f *scribble-preprocessor*))
    (if f (funcall f x) x)))

(defmacro with-preprocessor (pp &body body)
  "Form to define local Scribble preprocessor"
  `(let ((*scribble-preprocessor* ,pp)) ,@body))

(defun scribble-preprocess (s)
  (etypecase *scribble-preprocess*
    (null s)
    ((eql t) `(pp ,s))
    ((or symbol function) (funcall *scribble-preprocess* s))))

;-----------------------------------------------------------------------------
;;; Customizing components combination

(defvar *scribble-list* 'default-scribble-list
   "Scribble customization parameter: you can change it so as to define what
scribble returns from the list of components in parsed bracketed text")

(defparameter *scribble-default-head* 'cl:list
   "Scribble customization parameter: assuming default scribble-list behaviour,
modify the head of the form returned to combine at runtime the multiple
components of the bracketed text being parsed")

(defun default-scribble-list (&rest list)
   "Default behaviour for returning components of bracketed text"
   (if (null (cdr list)) (car list) ; returns nil when no components
     (apply 'do-scribble-list list)))

(defun do-scribble-list (&rest list)
   "Combine components of bracketed text at runtime
with *scribble-default-head*"
   (cons *scribble-default-head* list))

(defun scribble-list (&rest list)
  (apply *scribble-list* list))

; -----------------------------------------------------------------------------
;;; Customizing bracket-colon syntax

(defparameter *scribble-package* nil
  "if not NIL, the package in which Scribble will read
the head of text in bracket-colon syntax")

(defmacro within-package (package &body body)
  "do stuff while binding *package* to package if not NIL at runtime"
  `(let ((package ,package) (fun #'(lambda () ,@body)))
     (if package
	 (let ((*package* (find-package package))) (funcall fun))
       (funcall fun))))

(defmacro within-scribble-package (&body body)
  `(within-package *scribble-package* ,@body))

(defparameter *scribble-cons* 'default-scribble-cons
   "Scribble customization parameter: you can change it so as to define what
scribble returns from the head and body of text in bracket-colon syntax")

(defun scribble-cons (head body)
  (funcall *scribble-cons* head body))

(defun ensure-list (foo)
  (if (listp foo) foo (list foo)))

(defun default-scribble-cons (head body)
   (append (ensure-list head) body))

(defun scribble-cons-with-list-head (head body)
   (cons (ensure-list head) body))

; -----------------------------------------------------------------------------
;;; Some error handling
(defun issue-parse-error (fmt &rest r)
    #+sbcl (error 'sb-int:simple-parse-error
		  :format-control fmt
		  :format-arguments r)
    #-sbcl (apply 'error fmt r))

; -----------------------------------------------------------------------------
;;; The META parser

(deftype spacing-character ()
  "spacing character"
  '(member #\space #\newline #\tab #\linefeed #\return #\page))

(eval-when (:compile-toplevel :load-toplevel :execute)
 (enable-meta-syntax))
(defun parse-bracket (stream &aux c (s (make-string-output-stream)) (l '()))
  (with-stream-meta (st stream)
   (labels
    ((head ()
       (match
        { [#\: !(let* ((head (within-scribble-package
			      (read-preserving-whitespace st t t t)))
		       (ignore (skip-spaces))
		       (body (body)))
		  (declare (ignore ignore))
		  (scribble-cons head body))]
          !(apply 'scribble-list (body)) }))
     (add-char (c)
       (write-char c s))
     (flush ()
       (add-string (get-output-stream-string s)))
     (add-string (s)
       (or (= (length s) 0)
	   (add (scribble-preprocess s))))
     (add (x)
       (or (null x)
	   (push x l)))
     (skip-spaces ()
       (match {[@(spacing-character c) !(skip-spaces)]}))
     (body ()
       (match
        {[#\[ !(issue-parse-error
		"Nested bracket neither after backslash or comma on ~A @ ~A."
		stream (file-position stream))]
	 [#\] !(progn
		(flush)
		(close s)
		(return-from body (reverse l)))]
	 [#\, { [#\( !(progn (flush)
			     (add (read-delimited-list #\) st t))
			     (body))]
	        [#\, !(progn (flush)
			     (unread-char #\, st)
			     (add (read-preserving-whitespace st t t t))
			     (body))]
	        [#\[ !(progn (flush)
			     (add (parse-bracket st))
			     (body))]
	        !(progn (add-char #\,) (body)) }]
	 [#\\ @(character c) !(progn (add-char c) (body))]
	 [@(character c) !(progn (add-char c) (body))]})))
    (head))))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (disable-meta-syntax))


; -----------------------------------------------------------------------------
;;; readtable processing

;Temporary readtable stuff
(defvar *saved-readtables* '())
(defun push-readtable (&optional readtable)
  (push *readtable* *saved-readtables*)
  (setf *readtable* (cond ((null readtable) (copy-readtable *readtable*))
			  ((readtablep readtable) readtable)
			  (t (copy-readtable nil)))))
(defun pop-readtable ()
  (setf *readtable* (pop *saved-readtables*)))
;(defvar *standard-readtable* (copy-readtable nil))

;; Making a new table with scribble extension
(defvar *scribble-readtable* nil)
(defun enable-scribble-syntax (&optional readtable)
  (setf *scribble-readtable* (push-readtable readtable))
  (set-macro-character #\]
      #'(lambda (stream char)
      (declare (ignore char))
      (issue-parse-error "] outside of a [ construct on ~A @ ~A." stream (file-position stream))))
  (set-macro-character #\[
      #'(lambda (stream char)
      (declare (ignore char))
      (parse-bracket stream)))
  *scribble-readtable*)
(defun disable-scribble-syntax ()
  (pop-readtable))
(defun reenable-scribble-syntax ()
  (if (readtablep *scribble-readtable*)
      (enable-scribble-syntax)
    (push-readtable *scribble-readtable*))
  *scribble-readtable*)

;; Alternate syntax under dispatching-macro-character #\#
(defvar *sub-scribble-readtable* nil)
(defun enable-sub-scribble-syntax (&optional readtable)
  (setf *sub-scribble-readtable* (push-readtable readtable))
  (set-macro-character #\]
      #'(lambda (stream char)
      (declare (ignore char))
      (issue-parse-error "] outside of a #[ construct on ~A @ ~A." stream (file-position stream))))
  (set-dispatch-macro-character #\# #\[
      #'(lambda (stream subchar arg)
	  (declare (ignore subchar arg))
	  (parse-bracket stream)))
  *sub-scribble-readtable*)
(defun disable-sub-scribble-syntax ()
  (pop-readtable))
(defun reenable-sub-scribble-syntax ()
  (if (readtablep *sub-scribble-readtable*)
      (enable-sub-scribble-syntax)
    (push-readtable *sub-scribble-readtable*))
  *sub-scribble-readtable*)

; -----------------------------------------------------------------------------
;;; Configuring Scribble for use with various other systems
; These functions may not have been tested.
; Check http://www.cliki.net for more on the below packages.
; Please send me working versions of these functions.
; Note that you must still independently (enable-scribble-syntax)
; or (enable-sub-scribble-syntax).

(defun configure-scribble (&key (preprocess nil)
				(preprocessor nil)
				(list 'default-scribble-list)
				(default-head 'list)
				(package nil)
				(cons 'default-scribble-cons))
  (setf *scribble-preprocess* preprocess
	*scribble-preprocessor* preprocessor
	*scribble-list* list
	*scribble-default-head* default-head
	*scribble-package* package
	*scribble-cons* cons)
  t)

#|
(defun configure-scribble-for-exscribe ()
  "This will make Scribble work with exscribe"
  (configure-scribble :package :exscribe-user
		      :cons 'default-scribble-cons
		      :list 'default-scribble-list
		      :default-head 'klist
		      :preprocess t
		      :preprocessor nil))
|#

(defun configure-scribble-for-araneida ()
  "This will make Scribble work with the patched version of araneida's original html.lisp function that I used in CTO and that handles 'list correctly. Hopefully my patch will be integrated into the main upstream darcs repository."
  (configure-scribble :cons 'scribble-cons-with-list-head))

(defun configure-scribble-for-htmlgen ()
  "This is meant to make Scribble work with AllegroServe's HTMLGEN from Franz, Inc. -- a least if I read the spec correctly."
  (configure-scribble :cons 'cons
		      :default-head (read-from-string "net.html.generator:html")
		      :package (find-package '#:keyword)))

(defun configure-scribble-for-lml2 ()
  "This makes Scribble work with LML2 by kmr,
which is based on Franz's HTMLGEN."
  (configure-scribble :default-head (read-from-string "lml2:html")
		      :package (find-package '#:keyword)
		      :cons 'cons))

(defun configure-scribble-for-tml ()
  "tml, previously known as htout, is tfeb's package.
This is a wild guess from reading the docs.
Please modify to actually suit the package."
  (configure-scribble :default-head (read-from-string "org.tfeb.tml:htm")
		      :package (find-package '#:keyword)
		      :cons 'cons))

(defun configure-scribble-for-who ()
  "WHO is an optimized html generation package by Edi Weitz.
Its keyword semantics is very Scribe-like.
I wrote this reading the docs, but didn't test it."
  (configure-scribble :default-head (read-from-string "who:htm")
		      :package (find-package '#:keyword)))

(defun configure-scribble-for-yaclml ()
  "yaclml is yet another common lisp markup language.
The author wrote this support, but didn't test it."
  (configure-scribble :default-head (read-from-string "yaclml:yaclml-quote")
		      :package (find-package '#:it.bese.yaclml)
		      :cons 'cons))
