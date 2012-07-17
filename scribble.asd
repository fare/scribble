;;; -*- Lisp -*-
(defsystem scribble
  :description "Syntax extensions akin to Racket's Scribble and Bigloo's Skribe"
  :long-description "Scribble offers two syntax extensions allowing you to very nicely
mix text markup data and Lisp code. One syntax is a port to CL of Racket's Scribble syntax,
and the other is a port to CL of Bigloo's Skribe syntax."
  :depends-on (:meta :fare-utils :fare-matcher :fare-memoization
               :fare-quasiquote-readtable :named-readtables)
  :components
  ((:file "package")
   (:file "utilities" :depends-on ("package"))
   (:file "stream-line-column" :depends-on ("package"))
   (:file "racket" :depends-on ("package"))
   (:file "skribe" :depends-on ("utilities"))
   (:file "readtables" :depends-on ("racket" "skribe"))))
