;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;; Scribble: Racket-like scribble reader extension for Common Lisp

;; See Racket documentation: http://docs.racket-lang.org/scribble/reader.html
;; And racket source code: pkgs/at-exp-lib/scribble/reader.rkt

#+xcvb (module (:depends-on ("package")))

(in-package :scribble)

(eval-now

(defun parse-at-syntax (input)
  ;; Parse an @ expression.
  (with-nesting ()
    (with-input (input))
    (let* ((o (make-string-output-stream)) ; buffered output of "current stuff"
           (i (make-instance 'buffered-input :stream input))
           (cmdonly nil)
           (col 0)
           (line ())
           (lines ())
           (mrof '()))) ; current form (reversed)
    (labels ;; functions starting with ? process input after matching what is described in the name,
        ;; e.g. ?at processes input after an at-sign @.
        ;; those ending with ! issue output.
        ((?at () ; what to do after a @
           (cond
             ((expect-char i #.(coerce '(#\space #\tab #\return #\newline) 'base-string))
              (simple-parse-error "Unexpected whitespace after @"))
             ((expect-char i #\;)
              (?at-comment))
             (t
              (?punctuation))))
         (?at-comment () ; what to do after @;
           (cond
             ((expect-char i #\{) (?{text}))
             (t (read-line i)))
           (flush-buffer i)
           (read-preserving-whitespace input t nil nil))
         (?punctuation ()
           (let ((char (expect-char i "'`,")))
             (ecase char
               ((#\') (?quote))
               ((#\`) (?backquote))
               ((#\,) (cond
                        ((expect-char i #\@)
                         (?comma-at))
                        ((expect-char i #\.)
                         (?comma-dot))
                        (t
                         (?comma))))
               ((nil) (?cmd)))))
         (?quote ()
           (kwote (?punctuation)))
         (?backquote ()
           (call-with-quasiquote-reader #'?punctuation))
         (?comma-at ()
           (call-with-unquote-splicing-reader #'?punctuation))
         (?comma-dot ()
           (call-with-unquote-nsplicing-reader #'?punctuation))
         (?comma ()
           (call-with-unquote-reader #'?punctuation))
         (?cmd ()
           (let ((char (expect-char i "|[{")))
             (case char
               ((#\|)
                (?maybe-alttext #'?at-pipe))
               ((#\[ #\{)
                (?datatext char))
               (t
                (?cmd1)))))
         (?maybe-alttext (cont)
           (unread-char* i #\|)
           (let ((k (?newkey)))
             (cond
               (k
                (setf cmdonly nil)
                (?{alttext} k))
               (t
                (funcall cont)))))
         (?at-pipe ()
           (read-char* i)
           (let ((r (read-to-char #\| i))
                 (eof '#:eof))
             (multiple-value-bind (s n) (read-from-string r)
               (unless (eq eof (ignore-errors (read-from-string r nil eof :start n)))
                 (simple-parse-error "Unexpected characters in ~S after position ~D" r n))
               (setf cmdonly t)
               (form! s)
               (?end))))
         (?cmd1 ()
           (setf cmdonly t)
           (flush-buffer i)
           (form! (read-preserving-whitespace input t nil nil))
           (?cmd2))
         (?cmd2 ()
           (let ((char (expect-char i "[{|")))
             (if char
                 (?datatext char)
                 (?end))))
         (form! (x)
           (push x mrof))
         (?datatext (char)
           (ecase char
             (#\[ (?[data]))
             ((#\{ #\|) (unread-char* i char) (?{text}0))))
         (?[data] ()
           (setf cmdonly nil)
           (flush-buffer i)
           (map () #'form! (read-delimited-list #\] input t))
           (?{text}0))
         (?{text}0 ()
           (cond
             ((expect-char i #\{)
              (setf cmdonly nil)
              (?{text}))
             ((expect-char i #\|)
              (?maybe-alttext #'?end))
             (t (?end))))
         (?newkey ()
           (loop
             :with nil = (expect-char i #\|)
             :for c = (expect-char i)
             :while (and (ascii-punctuation-char-p c) (not (find c "@|{")))
             :collect c :into l
             :finally (cond
                        ((eql c #\{) (return (coerce l 'base-string)))
                        (t (unread-string i l) (return nil)))))
         (char! (c)
           (write-char c o))
         (flush! ()
           (let* ((s (get-output-stream-string o)))
             (when (plusp (length s))
               (push s line))))
         (eol! (eol)
           (let* ((s (get-output-stream-string o))
                  (s (if eol (trim-ending-spaces s) s)))
             (when (plusp (length s))
               (push s line))
             (push (cons col (reverse line)) lines))
           (when eol
             (setf col (skip-whitespace-return-column i 0)
                   line ()))
           t)
         (?{text} (&aux (brace-level 1))
           (flush-buffer i)
           (setf col (stream-line-column-harder input)
                 line ())
           (loop :for c = (expect-char i) :do
             (case c
               ((#\return)
                (expect-char i #\newline)
                (eol! t))
               ((#\newline)
                (eol! t))
               ((#\{)
                (incf brace-level)
                (char! c))
               ((#\@)
                (?inside-at))
               ((#\})
                (decf brace-level)
                (cond
                  ((zerop brace-level)
                   (eol! nil)
                   (flush-text!)
                   (return (?end)))
                  (t
                   (char! c))))
               (otherwise
                (char! c)))))
         (?inside-at ()
           (let ((c (expect-char i ";\"|")))
             (case c
               ((#\;)
                (cond
                  ((expect-char i #\{)
                   (let ((m mrof) (l line) (ls lines) (c col) (co cmdonly) (oo o))
                     (setf o (make-string-output-stream))
                     (?{text})
                     (setf mrof m line l lines ls col c cmdonly co o oo)))
                  (t
                   (flush-buffer i)
                   (read-line input)
                   (skip-whitespace-return-column input))))
               ((#\")
                (unread-char* i #\")
                (flush-buffer i)
                (write-string (read-preserving-whitespace input t nil nil) o))
               ((#\|)
                (flush!)
                (let ((r (read-to-char #\| i)))
                  (with-input-from-string (s r)
                    (loop :for x = (read-preserving-whitespace s nil s nil)
                      :until (eq x s) :do (push x line)))))
               (otherwise
                (flush!)
                (flush-buffer i)
                (push (parse-at-syntax input) line)))))
         (flush-text! ()
           (let* ((mincol (loop :for (col . strings) :in lines
                                :when strings :minimize col))
                  (text (loop :for (col . strings) :in (reverse lines)
                          :for first = t :then nil
                          :append
                          `(,@(when (and strings (> col mincol) (not first))
                                    (list (n-spaces (- col mincol))))
                              ,@strings ,*lf*))))
             (when (eq *lf* (first text))
               (pop text))
             (let ((e (every (lambda (x) (eq x *lf*)) text))
                   (r (reverse text)))
               (unless e
                 (loop :repeat 2 :when (eq *lf* (first r)) :do (pop r)))
               (setf mrof (append r mrof))))
           t)
         (?{alttext} (key)
           (let ((brace-level 1)
                 (rkey (mirror-string key)))
             (flush-buffer i)
             (setf col (stream-line-column-harder input)
                   line ())
             (loop :for c = (expect-char i) :do
               (case c
                 ((#\return)
                  (expect-char i #\newline)
                  (eol! t))
                 ((#\newline)
                  (eol! t))
                 (#\|
                  (if (not (expect-string i key)) (char! #\|)
                      (let ((c (expect-char i "@{")))
                        (case c
                          ((#\{)
                           (incf brace-level)
                           (char! #\|)
                           (map () #'char! key)
                           (char! c))
                          ((#\@)
                           (?inside-at))
                          (otherwise
                           (unread-string i key)
                           (char! #\|))))))
                 ((#\})
                  (cond
                    ((not (expect-string i rkey)) (char! #\}))
                    ((expect-char i #\|)
                     (decf brace-level)
                     (cond
                       ((zerop brace-level)
                        (eol! nil)
                        (flush-text!)
                        (return (?end)))
                       (t
                        (char! #\})
                        (map () #'char! rkey)
                        (char! #\|))))
                    (t (unread-string i rkey) (char! #\}))))
                 (otherwise
                  (char! c))))))
         (?end ()
           (flush-buffer i)
           (if (and cmdonly (length=n-p mrof 1))
               (car mrof)
               (reverse mrof)))))
    (?at))) ;; a @ character was just read by who called this function, so start parsing at ?at

(defun read-at-syntax (stream &optional char)
  (declare (ignore char))
  (parse-at-syntax stream))
(defun forbidden-pipe-macro (stream char)
  (declare (ignore stream char))
  (simple-parse-error "| not allowed when at syntax enabled"))

(defun do-enable-scribble-at-syntax (&key (table *readtable*) scribe skribe)
  (enable-quasiquote :table table)
  (flet ((s (char fun) (set-macro-character char fun nil table)))
    (s #\[ #'read-paren-list)
    (s #\] #'unbalanced-paren)
    (s #\{ #'read-paren-list)
    (s #\} #'unbalanced-paren)
    (s #\@ #'read-at-syntax)
    (when (or scribe skribe) ;; backward compatibility with former scribble?
      (do-enable-scribble-syntax table))
    (s #\| #'forbidden-pipe-macro))
  t)

(defvar *scribble-at-readtable* nil)
(defun enable-scribble-at-syntax (&key (table *readtable*) (scribe nil))
  (setf *scribble-at-readtable* (push-readtable table))
  (do-enable-scribble-at-syntax :table *scribble-at-readtable* :scribe scribe)
  *scribble-at-readtable*)
(defun disable-scribble-at-syntax ()
  (pop-readtable))
(defun reenable-scribble-at-syntax (&key scribe)
  (if (readtablep *scribble-at-readtable*)
      (enable-scribble-at-syntax :scribe scribe)
    (push-readtable *scribble-at-readtable*))
  *scribble-at-readtable*)

(defun parse-at-string (x)
  (with-input-from-string (i x)
    (let ((*readtable* *scribble-at-readtable*))
      (scribble::parse-at-syntax i))))

);eval-now
