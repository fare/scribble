;;; -*- Lisp -*-
(in-package :keyword)

(asdf:defsystem scribble
  depends-on (closer-mop meta fare-utils fare-matcher)
  serial t
  components (;;(file "ll")
              (file "package")
	      (file "scribble")))
