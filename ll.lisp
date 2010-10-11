;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;; LL(\omega) parser: indefinite lookahead recursive descent parsing.

#+xcvb (module (:depends-on ("package")))

(defpackage :llω
  (:nicknames :ll-omega)
  (:use :closer-common-lisp :closer-mop :pure)
  (:export
   #:accept))

(in-package :ll-omega)

;;; First, a datastructure for buffering from a stream,
;;; marking positions in the stream with some data
;;; used for backtracking (e.g. a continuation);
;;; remembering all characters up to the oldest mark.

(defparameter $buffer-size 1024)

(defun make-ωs-buffer (&key (size $buffer-size) (buffer "") (start 0) (end 0))
  (let* ((fill-pointer (- end start))
         (initial-contents
          (make-array fill-pointer :displaced-to buffer :displaced-index-offset start)))
    (make-array size :element-type 'character
                :adjustable t :fill-pointer fill-pointer
                :initial-contents initial-contents)))

(defclass ωs ()
  ((stream
    :type t #|stream|# :accessor ωs-stream :initarg :stream
    :documentation "underlying unbuffered stream")
   (buffer
    :type string :accessor ωs-buffer :initform (make-ωs-buffer)
    :documentation "buffer for memoized input")
   (position
    :type (integer 0 *) :accessor ωs-position :initform 0
    :documentation "current position or cursor in the stream")
   (offset
    :type (integer 0 *) :accessor ωs-offset :initform 0
    :documentation "position of buffer in the stream")
   (oldest-mark
    :type (integer 0 *) :accessor ωs-oldest-mark :initform 0
    :documentation "position of oldest mark, when there are any, not meaningful otherwise")
   (marks
    :type list :accessor ωs-marks :initform nil
    :documentation "set of markers on the buffer, can easily push, test for emptiness"))
  (:documentation "Omega stream, indefinitely buffered from user-specified marks"))

(defvar *ωs* nil
  "default omega-stream to work on")

(defmethod check-invariant! ((ωs ωs))
  (with-slots (buffer position offset oldest-mark marks) ωs
    (assert (adjustable-array-p buffer))
    (assert (<= 0 (- position offset) (fill-pointer buffer)))
    (dolist (m marks)
      (check-type m (cons (integer 0 *) (or function symbol))))
    (when marks
      (assert (<= offset oldest-mark position))))
  t)

(defmethod print-object ((ωs ωs) s)
  (print-unreadable-object (ωs s :type t)
    (with-slots (stream buffer position offset oldest-mark marks) ωs
      (let ((start (- (if marks oldest-mark position) offset))
            (end (fill-pointer buffer)))
        (format stream "~<~A :stream ~S :buffer ~S ~
			:position ~S :offset ~S :oldest-mark ~S :marks ~S~>"
                (if (ignore-errors (check-invariant! ωs)) "" "(violates invariant) ")
                s (ignore-errors (subseq buffer start end))
                position offset oldest-mark marks))))
  (values))

(defun ωs-mark! (continuation &optional (ωs *ωs*))
  (with-slots (position oldest-mark marks) ωs
    (unless marks
      (setf oldest-mark position))
    (push (cons position continuation) marks)
    position))

(defun ωs-unmark! (&optional (ωs *ωs*))
  (with-slots (marks) ωs
    (unless marks
      (error "Trying to reset an ωs without a mark"))
    (pop marks)))

(defun adjust-ωs-buffer (&optional (ωs *ωs*))
  ;(check-invariant ωs)
  (with-slots (buffer position offset oldest-mark marks) ωs
    (let ((fill-pointer (fill-pointer buffer))
          (total-size (length buffer)))
      (when (= fill-pointer total-size)
        (let* ((start (if marks oldest-mark position))
               (buffer-start (- start offset))
               (useful-size (- fill-pointer buffer-start))
               (new-size (ceiling (* 2 useful-size) $buffer-size)))
          (setf buffer (make-ωs-buffer :size new-size :buffer buffer
                                       :start buffer-start :end fill-pointer))))))
  (values))

(defun fill-ωs-buffer (&optional (ωs *ωs*))
  (adjust-ωs-buffer ωs)
  (with-slots (stream buffer) ωs
    (let ((start (fill-pointer buffer)))
      (setf (fill-pointer buffer) (length buffer))
      (let ((index (read-sequence buffer stream :start start)))
        (setf (fill-pointer buffer) index)
        (not (zerop (- index start))))))
  (values))

(defun ωs-peek-char (&optional (ωs *ωs*))
  (block nil
    (with-slots (stream buffer position offset) ωs
      (when (= (- position offset) (fill-pointer buffer))
        (unless (fill-ωs-buffer)
          (return nil)))
      (aref buffer (- position offset)))))

(defun ωs-read-char (&optional (ωs *ωs*))
  (let ((c (ωs-peek-char ωs)))
    (when c
      (incf (ωs-position ωs)))
    c))

(defun ωs-peek-n-chars (n &optional (ωs *ωs*))
  (with-slots (stream buffer position offset) ωs
    (loop :until (<= n (- (fill-pointer buffer) (- position offset))) :do
      (unless (fill-ωs-buffer)
        (return-from ωs-peek-n-chars nil)))
    (let* ((start (- position offset))
           (end (+ start n)))
      (subseq buffer start end))))

(defun ωs-read-n-chars (n &optional (ωs *ωs*))
  (let ((s (ωs-peek-n-chars n ωs)))
    (when s
      (incf (ωs-position ωs) n))
    s))
