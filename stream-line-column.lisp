;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;; stream-line-column

#+xcvb (module (:depends-on ("package")))

(in-package :scribble)

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
