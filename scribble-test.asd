;;; -*- Lisp -*-
(in-package :keyword)

(asdf:defsystem scribble-test
  depends-on (scribble hu.dwim.stefil babel)
  serial t
  components ((file "tests")))