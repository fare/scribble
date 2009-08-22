;;; -*- Lisp -*-
(in-package :keyword)

(asdf:defsystem scribble
  depends-on (:meta)
  serial t
  components ((file "package")
	      (file "scribble")))
