;;; -*- Lisp -*-
(defsystem scribble
  :description "Syntax extensions akin to Racket's Scribble and Bigloo's Skribe"
  :long-description "Scribble offers two syntax extensions allowing you to very nicely
mix text markup data and Lisp code. One syntax is a port to CL of Racket's Scribble syntax,
and the other is a port to CL of Bigloo's Skribe syntax."
  :depends-on (#|:closer-mop|# :meta :fare-utils :fare-matcher)
  :serial t
  :components
  (;;(file "ll")
   (:file "package")
   (:file "scribble-scribe")
   (:file "scribble")))
