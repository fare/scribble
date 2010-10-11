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
       (<= 127 (char-code x))))

(defun expected-char-p (c expectation)
  (check-type c (or null character))
  (etypecase expectation
    (null t)
    (character (eql c expectation))
    (sequence (find c expectation))
    (function (funcall expectation c))))

(defvar *lf* (string #\newline))

(memo:define-memo-function n-spaces (n)
  (make-string n :initial-element #\space :element-type 'base-character))

(defun expect-char (i &optional expectation)
  (let ((c (peek-char nil i nil nil t)))
    (and (expected-char-p c expectation) (read-char i))))

(defun parse-at-syntax (i)
  ;; Parse an @ expression.
  ;; returns multiple values: the parsed expression, and
  ;; some flags to be used by recursive @ calls.
  (with-nesting ()
    (let* (;;(i (make-instance 'ωs :stream stream)) ; buffered input
           (o (make-string-output-stream)) ; buffered output of "current stuff"
           (cmdonly nil)
           (mrof '()))) ; current form (reversed)
    (labels
        ((?@ () ; expect a @ expression
           (unless (expect-char i #\@)
             (error "Expected #\@"))
           (?@1))
         (?@1 () ; what to do after a @
           (?punctuation))
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
           (let ((char (expect-char i "[{|")))
             (if char
                 (?datatext char)
                 (?cmd1))))
         (?cmd1 ()
           (setf cmdonly t)
           (form! (read i))
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
              (unread-char #\| i)
              (let ((k (?newkey)))
                (cond
                  (k
                   (setf cmdonly nil)
                   (?{alttext} k))
                  (t
                   (?end)))))
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
                        (t (file-position i p) nil))))
         (char! (c)
           (write-char c o))
         (flush! ()
           (let ((s (get-output-stream-string o)))
             (when (plusp (length s))
               (form! s))))
         (?{text} ()
           (loop :with brace-level = 1
             ;; :with initial-col = (stream-line-column-harder i)
             :for c = (expect-char i) :do
             (case c
               ((#\{)
                (incf brace-level)
                (char! c))
               ((#\@)
                (NIY))
               ((#\})
                (decf brace-level)
                (when (zerop brace-level)
                  (flush!)
                  (return (?end))))
               (otherwise
                (NIY)))))
         (?{alttext} (key)
           (let ((keylen (length key))
                 (rkey (mirror-string key)))
             (NIY keylen rkey)))
         (?end ()
           (if (and cmdonly (length=n-p mrof 1))
               (car mrof)
               (reverse mrof))))
      (?@))))

(defun do-enable-scribble-at-syntax (&optional (readtable *readtable*))
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
  t)

(defvar *saved-readtable* *readtable*)

(defparameter *scribble-readtable*
  (let ((r (copy-readtable *saved-readtable*)))
    (do-enable-scribble-at-syntax r)
    r))

(defun parse-at-string (x)
  (with-input-from-string (i x)
    (let ((*readtable* *scribble-readtable*))
      (scribble::parse-at-syntax i))))
