;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
(in-package :scribble)

(eval-now

(define-condition simple-parse-error (simple-error parse-error)
  ())

(defun simple-parse-error (format &rest arguments)
  (error 'simple-parse-error
         :format-control format :format-arguments arguments))

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

(defun ascii-punctuation-char-p (x)
  (and (typep x 'base-char)
       (<= 33 (char-code x) 126)
       (not (alphanumericp x))))


; -----------------------------------------------------------------------------
;;; Distinguished strings

(defvar *lf* (string #\newline))

(fmemo:define-memo-function n-spaces (n)
  (make-string n :initial-element #\space :element-type 'base-char))


; -----------------------------------------------------------------------------
;;; Infinite look-ahead, required for @foo|----{ }----| processing.

(defgeneric peek-char* (input))
(defgeneric read-char* (input))
(defgeneric unread-char* (input char))

(defmethod peek-char* ((input stream))
  (peek-char nil input nil nil t))
(defmethod read-char* ((input stream))
  (read-char input nil nil))
(defmethod unread-char* ((input stream) char)
  (unread-char char input))

(defclass buffered-input ()
  ((stream :initarg :stream :type stream :reader input-stream)
   (buffer :initform (make-array '(16) :element-type 'character :adjustable t :fill-pointer 0) :reader input-buffer)))
(defmethod peek-char* ((input buffered-input))
  (let ((c (read-char* input)))
    (when c (unread-char* input c))
    c))
(defmethod read-char* ((input buffered-input))
  (let ((b (input-buffer input)))
    (if (plusp (fill-pointer b)) (vector-pop b)
        (read-char* (input-stream input)))))
(defmethod unread-char* ((input buffered-input) char)
  (vector-push-extend char (input-buffer input)))
(defgeneric flush-buffer (input))
(defmethod flush-buffer ((input buffered-input))
  (let* ((b (input-buffer input))
         (p (fill-pointer b)))
    (assert (<= p 1))
    (loop :for i :from (1- p) :downto 0 :do
      (unread-char* (input-stream input) (aref b i))
          :finally (setf (fill-pointer b) 0))))

(defun unread-string (input string)
  (loop :for char :across (reverse string) :do (unread-char* input char)))


; -----------------------------------------------------------------------------
;;; Basic parsing

(defun expected-char-p (c expectation)
  (check-type c (or null character))
  (etypecase expectation
    (null t)
    (character (eql c expectation))
    (sequence (find c expectation))
    (function (funcall expectation c))))

(defun expect-char (input &optional expectation)
  (let ((c (peek-char* input)))
    (and (expected-char-p c expectation) (read-char* input))))

(defun expect-string (i s)
  (loop :for c :across s :for l :from 0 :do
    (unless (expect-char i c)
      (unread-string i (subseq s 0 l))
      (return (values nil l)))
    :finally (return (values t (length s)))))

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

(defun read-paren-list (stream opening)
  (let ((closing (mirror-char opening)))
    (check-type closing character)
    (read-delimited-list closing stream t)))

(defun unbalanced-paren (stream char)
  (simple-parse-error "Unbalanced ~A on ~A @ ~A." char stream (file-position stream)))

);eval-now
