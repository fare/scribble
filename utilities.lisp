;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
#+xcvb (module (:depends-on ("package")))

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

(fmemo:define-memo-function n-spaces (n)
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

(defun read-paren-list (stream opening)
  (let ((closing (mirror-char opening)))
    (check-type closing character)
    (read-delimited-list closing stream t)))

(defun unbalanced-paren (stream char)
  (simple-parse-error "Unbalanced ~A on ~A @ ~A." char stream (file-position stream)))

);eval-now
