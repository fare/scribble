;;; -*- Lisp -*-

(defsystem :scribble-test
  :depends-on (:scribble :hu.dwim.stefil babel)
  :serial t
  :components ((:file "tests")))
