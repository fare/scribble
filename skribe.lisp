;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;; See README.
(in-package :scribble)
(named-readtables:in-readtable :meta)

(eval-now
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
in your macros so as to customize the behavior of preprocessing"
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
   "Scribble customization parameter: assuming default scribble-list behavior,
modify the head of the form returned to combine at runtime the multiple
components of the bracketed text being parsed")

(defun default-scribble-list (&rest list)
   "Default behavior for returning components of bracketed text"
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

(defun default-scribble-cons (head body)
   (append (ensure-list head) body))

(defun scribble-cons-with-list-head (head body)
   (cons (ensure-list head) body))

; -----------------------------------------------------------------------------
;;; The META parser

(deftype spacing-character ()
  "spacing character"
  '(member #\space #\newline #\tab #\linefeed #\return #\page))

(defun parse-bracket (stream &aux c (s (make-string-output-stream)) (l '()))
  (with-stream-meta (st stream)
   (labels
    ((head ()
       (match
        { [#\: !(let* ((head (within-scribble-package
			      (read-preserving-whitespace st t t nil)))
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
        {[#\[ !(simple-parse-error
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
			     (add (read-preserving-whitespace st t t nil))
			     (body))]
	        [#\[ !(progn (flush)
			     (add (parse-bracket st))
			     (body))]
	        !(progn (add-char #\,) (body)) }]
	 [#\\ @(character c) !(progn (add-char c) (body))]
	 [@(character c) !(progn (add-char c) (body))]})))
    (head))))


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
  (do-enable-scribble-syntax *scribble-readtable*)
  *scribble-readtable*)

(defun read-skribe-bracket (stream char)
  (declare (ignore char))
  (parse-bracket stream))

(defun do-enable-scribble-syntax (&optional readtable)
  (set-macro-character #\] #'unbalanced-paren nil readtable)
  (set-macro-character #\[ #'read-skribe-bracket nil readtable)
  t)
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
      (simple-parse-error "] outside of a #[ construct on ~A @ ~A." stream (file-position stream))))
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

(named-readtables:in-readtable :standard)
);eval-now
