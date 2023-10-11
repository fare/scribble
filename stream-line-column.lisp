;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;; stream-line-column
(in-package :scribble)

(defparameter $columns-per-tab 8
  "Number of columns in a text tab")

(defun ceiling-align (integer divisor)
  "Round the INTEGER up to a multiple of the DIVISOR"
  (* divisor (ceiling integer divisor)))

(defun to-next-tab (column &optional (columns-per-tab $columns-per-tab))
  "Given a COLUMN and the COLUMNS-PER-TAB (defaulting to $COLUMNS-PER-TAB,
return the column of the next tab, as reached a #\tab character"
  (ceiling-align (1+ column) columns-per-tab))

(defun char-crlf-p (char)
  "Is this character CR (ASCII 13) or LF (ASCII 10)?"
  (or (eql char #\return) (eql char #\linefeed)))

(defun string-column-modifier (string)
  "Return two values describing the effect of the STRING on column position:
1- UNALIGNED-ADJUSTMENT, which is NIL if there is a newline,
   but if there isn't, is an integer number of characters preceding any TAB.
2- ALIGNED-ADJUSTMENT, which is the number of columns following a newline,
   or the number of columns following the first tab if there is one but no newline,
   or NIL if there were neither newline nor tab"
  ;; TODO: handle double-width characters????
  (let ((l (length string)))
    (labels ((aligned-adjustment (delimpos)
               (loop :with adjustment = 0
                     :with pos = (1+ delimpos)
                     :for tabpos = (position #\tab string :start pos) :do
                       (if tabpos
                           (setf adjustment (to-next-tab (+ adjustment (- tabpos pos)))
                                 pos (1+ tabpos))
                           (return (+ adjustment (- l pos)))))))
      (if-let (nlpos (position-if #'char-crlf-p string :from-end t))
        (values nil (aligned-adjustment nlpos))
        (if-let (tabpos (position #\tab string))
          (values tabpos (aligned-adjustment tabpos))
          (values l nil))))))

(defun combine-column-modifiers (unaligned1 aligned1 unaligned2 aligned2)
  "Given the UNALIGNED1 and ALIGNED1 column modifiers of a first string,
and the UNALIGNED2 and ALIGNED2 column modifiers of a second string, return as
two values the UNALIGNED and ALIGNED column modifiers of their concatenation."
  (cond
    ;; String 2 had a newline:
    ;; What was before the newline, including string 1, doesn't matter.
    ((null unaligned2)
     (values nil aligned2))
    ;; String 1 had a newline or a tab, String 2 had no newline:
    ;; All of String 2 only continues the aligned adjustment of String 1.
    (aligned1
     (let ((aligned3 (+ aligned1 unaligned2)))
       (values unaligned1 (if aligned2 (+ (to-next-tab aligned3) aligned2) aligned3))))
    ;; String 1 had neither newline nor tab, String 2 had no newline:
    ;; Combine the unaligned and use string 2's aligned in case it has a tab.
    (t
     (values (+ unaligned1 unaligned2) aligned2))))

(defun adjust-stream-forward-to-char (stream)
  "Synchronize a file STREAM to the next valid character position.
UTF-8 encoding may make the current file-position invalid, but by repeatedly
trying to read a char at successive positions, we will consume all the
'wrong' bytes until we eventually reach a point where a character can be read
or we reach EOF."
  ;; At least on SBCL, on a STRING-INPUT-STREAM, FILE-POSITION actually sets
  ;; the character index within the input stream, and the adjustment will trivially work.
  ;; On other implementations, we assume that if FILE-POSITION doesn't work,
  ;; we are already aligned to a character.
  (if-let (start (ignore-errors (file-position stream)))
    (loop :for pos :from start
          :for nil = nil :then (file-position stream pos)
          :for c = (ignore-errors (read-char stream nil t nil))
          :until c
          :finally (progn (when (characterp c) (unread-char c stream)) (return pos)))))

(defun read-stream-to-pos (stream endpos)
  "Read the contents of a file stream from current position to endpos excluded"
  (declare (optimize (speed 1) (safety 3) (debug 3)))
  (loop :with startpos = (file-position stream)
    :with curpos = startpos
    :with startchar = 0
    :with endchar = (- endpos startpos) ;; each char takes at least one position
    :with buffer = (make-string endchar :initial-element #\_)
    :until (= startchar endchar) :do ;; dichotomy
    (loop :with curchar = endchar
          :for i = (read-sequence buffer stream :start startchar :end curchar)
          :for p = (file-position stream) :do
          (when (<= p endpos)
            (setf startchar curchar curpos p)
            (when (= p endpos)
              (setf endchar curchar))
            (return))
          ;; (> p endpos)
          (file-position stream curpos)
          (setf endchar curchar
                curchar (ash (+ startchar curchar) -1))
          (when (= startchar curchar)
            (setf endchar curchar)
            (return)))
    :finally
       (return (subseq buffer 0 endchar))))

(defun stream-line-column-harder (stream)
  "Extract the column we are at from the stream.
Ask the implementation gently, but if it won't tell,
reconstitute the data by reading previous characters
until we reach a beginning of line or of the who file.
"
  (or (ignore-errors (stream-line-column stream))
      (loop
        :with orig-pos = (file-position stream)
        :for targetpos = orig-pos :then adjustedstartpos
        :for range = 128 :then (* range 2)
        :for start = (max 0 (- targetpos range))
        :for startpos = (progn (file-position stream start)
                               (adjust-stream-forward-to-char stream))
        :for adjustedstartpos = (file-position stream)
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
                     (multiple-value-setq (unaligned aligned)
                       (combine-column-modifiers nil 0 unaligned aligned)))
                   (assert (null unaligned))
                   (file-position stream orig-pos)
                   (return aligned)))))
