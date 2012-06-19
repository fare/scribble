;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;; See README.
#+xcvb (module (:depends-on ("racket" "skribe")))

(in-package :scribble)

(eval-now
  (defreadtable :scribble-racket-mixin
    (:merge :fare-quasiquote-mixin)
    (:macro-char #\[ #'read-paren-list)
    (:macro-char #\] #'unbalanced-paren)
    (:macro-char #\{ #'read-paren-list)
    (:macro-char #\} #'unbalanced-paren)
    (:macro-char #\@ #'read-at-syntax)
    (:macro-char #\| #'forbidden-pipe-macro))

  (defreadtable :scribble-skribe-mixin
    (:macro-char #\[ #'read-skribe-bracket)
    (:macro-char #\] #'unbalanced-paren))

  (defreadtable :scribble-both-mixin
    (:fuze :scribble-racket-mixin :scribble-skribe-mixin))

  (defreadtable :scribble-racket
    (:fuze :standard :scribble-racket-mixin))

  (defreadtable :scribble-skribe
    (:fuze :standard :scribble-skribe-mixin))

  (defreadtable :scribble-both
    (:fuze :standard :scribble-both-mixin))

  (defreadtable :scribble
    (:merge :scribble-both)))

