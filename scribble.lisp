;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;; Scribble: Racket-like scribble reader extension for Common Lisp

#+xcvb (module (:depends-on ("package")))

(in-package :scribble)

; -----------------------------------------------------------------------------
;;; Optimization
(declaim (optimize (speed 2) (safety 3) (debug 3)))

; -----------------------------------------------------------------------------
;;; stream-line-column

(defparameter $columns-per-tab 8)

(defun to-next-tab (position &optional (columns-per-tab $columns-per-tab))
  (* columns-per-tab (ceiling (1+ position) columns-per-tab)))

(defun string-column-modifier (string)
  "Return multiple values describing the effect of the string on column position.
1- whether there was a newline found, if no NIL, if yes its position in the string.
2- if no newline, whether there is a leading tab that further aligns the column.
3- the number of characters after newline and/or tab."
  ;; TODO: handle double-width characters????
  (loop :with nlpos = (position #\newline string :from-end t)
    :with start = (if nlpos (1+ nlpos) 0)
    :with unaligned = (and (not nlpos) 0)
    :with aligned = (and nlpos 0)
    :for c :across (subseq string start) :do
    (if aligned
        (case c
          ((#\tab) (setf aligned (to-next-tab aligned)))
          ((#\return) (setf unaligned nil aligned 0))
          (t (incf aligned)))
        (case c
          ((#\tab) (setf aligned 0))
          ((#\return) (setf unaligned nil aligned 0))
          (t (incf unaligned))))
    :finally (return (values unaligned aligned nlpos))))

(defun combine-column-modifiers (unaligned1 aligned1
                                 unaligned2 aligned2)
  (cond
    ((null unaligned2) (values unaligned2 aligned2))
    ((null aligned1) (values (+ unaligned1 unaligned2) aligned2))
    ((null aligned2) (values unaligned1 (+ aligned1 unaligned2)))
    (t (values unaligned1 (+ (to-next-tab (+ aligned1 unaligned2)) aligned2)))))

(defun adjust-stream-forward-to-char (stream)
  (loop :for pos :from (file-position stream)
    :for nil = nil :then (file-position stream pos)
    :for c = (ignore-errors (read-char stream nil t nil))
    :until c
    :finally (progn (when (characterp c) (unread-char c stream)) (return pos))))

(defun read-stream-to-pos (stream endpos)
  (declare (optimize (speed 1) (safety 3) (debug 3)))
  (loop :with startpos = (file-position stream)
    :with maxchar = (- endpos startpos)
    :with buffer = (make-string maxchar :initial-element #\_)
    :with index = 0
    :until (zerop maxchar) :do ;; dichotomy
    (let* ((x (ceiling maxchar 2))
           (i (read-sequence buffer stream :start index :end (+ index x))))
      (if (= i index)
          (setf maxchar 0)
          (let ((p (file-position stream)))
            (if (<= p endpos)
                (setf index i
                      startpos p
                      maxchar (min (- maxchar x) (- endpos startpos)))
                (progn
                  (file-position stream startpos)
                  (setf maxchar (1- x)))))))
    :finally (return (subseq buffer 0 index))))

(defun stream-line-column-harder (stream)
  (or (ignore-errors (stream-line-column stream))
      (loop
        :with orig-pos = (file-position stream)
        :for targetpos = orig-pos :then startpos
        :for range = 128 :then (* range 2)
        :for start = (max 0 (- targetpos range))
        :for startpos = (progn (file-position stream start)
                               (adjust-stream-forward-to-char stream))
        :for string = (read-stream-to-pos stream targetpos)
        :for unaligned2 = 0 :then unaligned
        :for aligned2 = nil :then aligned
        :for (unaligned1 aligned1) =
        (multiple-value-list (string-column-modifier string))
        :for (unaligned aligned) =
        (multiple-value-list (combine-column-modifiers
                              unaligned1 aligned1 unaligned2 aligned2))
        ;;:for nil = (DBG :slch orig-pos targetpos range start startpos string unaligned2 aligned2 unaligned1 aligned1 unaligned aligned)
        :until (or (null unaligned) (zerop start))
        :finally (progn
                   (when (zerop start)
                     (multiple-value-setq (unaligned aligned) (combine-column-modifiers nil 0 unaligned aligned)))
                   (assert (null unaligned))
                   (return aligned)))))

; -----------------------------------------------------------------------------
;;; Markers

(defun mirror-char (x)
  (check-type x (or null character))
  (let* ((s "()[]{}<>pqbd")
         (p (position x s)))
    (if p
        (aref s (logxor p 1))
        x)))

(defun mirror-string (x)
  (check-type x string)
  (map 'string 'mirror-char (reverse x)))

(defun ascii-char-p (x)
  (and (typep x 'base-char)
       (<= (char-code x) 127)))

(defun expected-char-p (c expectation)
  (check-type c (or null character))
  (etypecase expectation
    (null t)
    (character (eql c expectation))
    (sequence (find c expectation))
    (function (funcall expectation c))))

(defvar *lf* (string #\newline))

(memo:define-memo-function n-spaces (n)
  (make-string n :initial-element #\space :element-type 'base-char))

(defun expect-char (i &optional expectation)
  (let ((c (peek-char nil i nil nil t)))
    (and (expected-char-p c expectation) (read-char i))))

(defun expect-string (i s)
  (loop :for c :across s :for l :from 0 :do
    (unless (expect-char i c)
      (return (values nil (subseq s l))))
    :finally (return (values t l))))

(defun skip-whitespace-return-column (i &optional (col 0))
  (loop :for c = (expect-char i #.(format nil " ~c" #\tab))
    :while c :do
    (ecase c
      ((#\space) (incf col))
      ((#\tab) (setf col (to-next-tab col))))
    :finally (return col)))

(defun trim-ending-spaces (s)
  (let ((p (position-if #'(lambda (c) (not (member c '(#\space #\tab)))) s :from-end t)))
    (if p (subseq s 0 (1+ p)) nil)))

(defun read-to-char (c &optional (i *standard-input*))
  (with-output-to-string (o)
    (loop :for char = (expect-char i)
      :until (eql c char)
      :do (write-char char o))))

(defun parse-at-syntax (i)
  ;; Parse an @ expression.
  (let* ((o (make-string-output-stream)) ; buffered output of "current stuff"
         (cmdonly nil)
         (col 0)
         (line ())
         (lines ())
         (mrof '())) ; current form (reversed)
    (labels
        ((?@1 () ; what to do after a @
           (cond
             ((expect-char i #.(coerce '(#\space #\tab #\return #\newline) 'base-string))
              (error "Unexpected whitespace after @"))
             ((expect-char i #\;)
              (?at-comment))
             (t
              (?punctuation))))
         (?at-comment ()
           (cond
             ((expect-char i #\{) (?{text}))
             (t (read-line i)))
           (read-preserving-whitespace i t nil nil))
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
                (maybe-alttext #'at-pipe))
               ((#\[ #\{)
                (?datatext char))
               (t
                (?cmd1)))))
         (maybe-alttext (cont)
           (unread-char #\| i)
           (let ((k (?newkey)))
             (cond
               (k
                (setf cmdonly nil)
                (?{alttext} k))
               (t
                (funcall cont)))))
         (at-pipe ()
           (read-char i)
           (let ((r (read-to-char #\| i))
                 (eof '#:eof))
             (multiple-value-bind (s n) (read-from-string r)
               (unless (eq eof (ignore-errors (read-from-string r nil eof :start n)))
                 (error "Unexpected characters in ~S after position ~D" r n))
               (setf cmdonly t)
               (form! s)
               (?end))))
         (?cmd1 ()
           (setf cmdonly t)
           (form! (read-preserving-whitespace i t nil nil))
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
             ((#\{ #\|) (unread-char char i) (?{text}0))))
         (?[data] ()
           (setf cmdonly nil)
           (map () #'form! (read-delimited-list #\] i t))
           (?{text}0))
         (?{text}0 ()
           (cond
             ((expect-char i #\{)
              (setf cmdonly nil)
              (?{text}))
             ((expect-char i #\|)
              (maybe-alttext #'?end))
             (t (?end))))
         (?newkey ()
           (loop
             :with p = (file-position i)
             :with nil = (expect-char i #\|)
             :for c = (expect-char i)
             :while (and (ascii-char-p c) (not (alphanumericp c)) (not (find c "@|{")))
             :collect c :into l
             :finally (cond
                        ((eql c #\{) (return (coerce l 'base-string)))
                        (t (file-position i p) (return nil)))))
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
           (setf col (stream-line-column-harder i)
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
                   (read-line i)
                   (skip-whitespace-return-column i))))
               ((#\")
                (unread-char #\" i)
                (write-string (read-preserving-whitespace i t nil nil) o))
               ((#\|)
                (flush!)
                (let ((r (read-to-char #\| i)))
                  (with-input-from-string (s r)
                    (loop :for x = (read-preserving-whitespace s nil s nil)
                      :until (eq x s) :do (push x line)))))
               (otherwise
                (flush!)
                (push (parse-at-syntax i) line)))))
         (flush-text! ()
           (let* ((mincol (loop :for (col . strings) :in lines
                            :when strings
                            :minimize col))
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
             (setf col (stream-line-column-harder i)
                   line ())
             (loop :for c = (expect-char i) :do
               (case c
                 ((#\return)
                  (expect-char i #\newline)
                  (eol! t))
                 ((#\newline)
                  (eol! t))
                 (#\|
                  (let* ((p (file-position i))
                         (c (and (expect-string i key) (expect-char i "@{"))))
                    (case c
                      ((#\{)
                       (incf brace-level)
                       (char! #\|)
                       (map () #'char! key)
                       (char! c))
                      ((#\@)
                       (?inside-at))
                      (otherwise
                       (file-position i p)
                       (char! #\|)))))
               ((#\})
                (let* ((p (file-position i)))
                  (cond
                    ((and (expect-string i rkey) (expect-char i #\|))
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
                    (t
                     (file-position i p)
                     (char! #\})))))
               (otherwise
                (char! c))))))
         (?end ()
           (if (and cmdonly (length=n-p mrof 1))
               (car mrof)
               (reverse mrof))))
      (?@1))))

(defun do-enable-scribble-at-syntax (&key (readtable *readtable*) scribe)
  (enable-quasiquote :readtable readtable)
  (set-macro-character
   #\[ #'(lambda (stream char)
           (declare (ignore char))
           (read-delimited-list #\] stream t))
   nil readtable)
  (set-macro-character
   #\] #'(lambda (stream char)
           (declare (ignore stream char))
           (error "Unbalanced ]"))
   nil readtable)
  (set-macro-character
   #\{ #'(lambda (stream char)
           (declare (ignore char))
           (read-delimited-list #\] stream t))
   nil readtable)
  (set-macro-character
   #\} #'(lambda (stream char)
           (declare (ignore stream char))
           (error "Unbalanced }"))
   nil readtable)
  (set-macro-character
   #\@ #'(lambda (stream char)
           (declare (ignore char))
           (parse-at-syntax stream))
   nil readtable)
  (when scribe ;; backward compatibility with former scribble?
    (do-enable-scribble-syntax readtable))
  (set-macro-character
   #\| #'(lambda (stream char)
           (declare (ignore stream char))
           (error "| not allowed when at syntax enabled"))
   nil readtable)
  t)

(defvar *scribble-at-readtable* nil)
(defun enable-scribble-at-syntax (&key (readtable *readtable*) (scribe nil))
  (setf *scribble-at-readtable* (push-readtable readtable))
  (do-enable-scribble-at-syntax :readtable *scribble-at-readtable* :scribe scribe)
  *scribble-at-readtable*)
(defun disable-scribble-syntax ()
  (pop-readtable))
(defun reenable-scribble-at-syntax (&key scribe)
  (if (readtablep *scribble-at-readtable*)
      (enable-scribble-at-syntax :scribe scribe)
    (push-readtable *scribble-at-readtable*))
  *scribble-readtable*)

(defun parse-at-string (x)
  (with-input-from-string (i x)
    (let ((*readtable* *scribble-at-readtable*))
      (scribble::parse-at-syntax i))))
