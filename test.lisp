(defpackage :scribble/test
  (:use :common-lisp :scribble :hu.dwim.stefil :fare-utils :uiop :named-readtables)
  (:import-from :scribble #:*lf*)
  (:export #:test-suite))

(in-package :scribble/test)

(defsuite* (test-suite :in root-suite :documentation "Testing scribble"))

(deftest test-column-modifier ()
  (macrolet ((ccm (x y z)
               `(is (equal (multiple-value-list
                            (combine-column-modifiers ,@x ,@y))
                           ',z)))
             (scm (x a b c)
               `(is (equal (multiple-value-list
                            (string-column-modifier ,(format nil x #\tab)))
                           '(,a ,b ,c)))))
    (scm "abcde" 5 nil nil)
    (scm "foo~%abcde" nil 5 3)
    (scm "foo bar~%abcde" nil 5 7)
    (scm "abcd~cfgh" 4 3 nil)
    (scm "foo bar~%abcde~cfgh" nil 11 7)
    (scm "~%abcde~cfgh" nil 11 0)
    (ccm (15 nil) (nil 23) (nil 23))
    (ccm (nil 15) (nil 23) (nil 23))
    (ccm (5 15) (nil 23) (nil 23))
    (ccm (15 nil) (23 nil) (38 nil))
    (ccm (nil 15) (23 nil) (nil 38))
    (ccm (5 15) (23 nil) (5 38))
    (ccm (nil 15) (23 42) (nil 82))
    (ccm (15 nil) (23 42) (38 42))
    (ccm (5 15) (23 42) (5 82))))


(defparameter *external-format*
  #-ccl :default
  #+ccl (ccl:make-external-format :character-encoding :utf-8))

(deftest test-file-position ()
  (with-temporary-file (:stream s :pathname p :direction :output)
    (princ "Hello, World
Faré λ 自由 foo
æéïôù
" s)
    :close-stream
  (with-open-file (s p :direction :input :external-format *external-format*)
    (is (file-length s) 42)
    (let ((cpos
           (loop :for p = (file-position s)
             :for st = "" :then (concatenate 'string st (string c))
             :for c = (read-char s nil nil)
             :for col = (multiple-value-bind (u a) (string-column-modifier st)
                          (nth-value 1 (combine-column-modifiers nil 0 u a)))
             ;;:for i :from 0 :for nil = (format t "~&pos ~2D ~2D ~2D ~S~%" i p col c)
             :while c
             :count (babel:string-size-in-octets (string c)) :into expectedp :do
             (is p expectedp)
             :collect (list c p st col))))
      (loop :for (c p st col) :in cpos :do
        (file-position s p)
        (is (eql (file-position s) p))
        (is (eql (read-char s) c))
        (file-position s 0)
        (is (equal (read-stream-to-pos s p) st))
        (is (eql (file-position s) p))
        (is (eql (stream-line-column-harder s) col))
        ))))
  t)

(deftest test-scribble-at ()
  ;; Tests taken from http://docs.racket-lang.org/scribble/reader.html
  (macrolet ((a (x y)
               `(is (equal (p ,x) ',(subst *lf* '*lf* y))))
             (a* (&rest r)
               `(flet ((p (x)
                         (let ((*readtable* (find-readtable :scribble)))
                           (read-from-string (strcat "      " x)))))
                  ,@(loop :for (x y) :on r :by #'cddr :collect `(a ,x ,y)))))
    (a*
     "@foo{blah blah blah}" (foo "blah blah blah")
     "@foo{blah \"blah\" (`blah'?)}" (foo "blah \"blah\" (`blah'?)")
     "@foo[1 2]{3 4}" (foo 1 2 "3 4")
     "@foo[1 2 3 4]" (foo 1 2 3 4)
     "@foo[:width 2]{blah blah}" (foo :width 2 "blah blah")
     "@foo{blah blah
           yada yada}" (foo "blah blah" *lf* "yada yada")
     "@foo{
        blah blah
        yada yada
     }" (foo "blah blah" *lf* "yada yada")
     "@foo{bar @baz{3}
           blah}" (foo "bar " (baz "3") *lf* "blah")
     "@foo{@b{@u[3] @u{4}}
           blah}" (foo (b (u 3) " " (u "4")) *lf* "blah")
     "@C{while (*(p++))
           *p = '\\n';}" (C "while (*(p++))" *lf* "  " "*p = '\\n';")
     "@{blah blah}" ("blah blah")
     "@{blah @[3]}" ("blah " (3))
     "'@{foo
         bar
         baz}" '("foo" *lf* "bar" *lf* "baz")
     "@foo" foo
     "@{blah @foo blah}" ("blah " foo " blah")
     "@{blah @:foo blah}" ("blah " :foo " blah")
     "@{blah @|foo|: blah}" ("blah " foo ": blah")
     "@foo{(+ 1 2) -> @(+ 1 2)!}" (foo "(+ 1 2) -> " (+ 1 2) "!")
     "@foo{A @\"string\" escape}" (foo "A string escape")
     "@foo{eli@\"@\"barzilay.org}" (foo "eli@barzilay.org")
     "@foo{A @\"{\" begins a block}" (foo "A { begins a block")
     "@C{while (*(p++)) {
           *p = '\\n';
         }}" (C "while (*(p++)) {" *lf* "  " "*p = '\\n';" *lf* "}")
     "@foo|{bar}@{baz}|" (foo "bar}@{baz")
     "@foo|{bar |@x{X} baz}|" (foo "bar " (x "X") " baz")
     "@foo|{bar |@x|{@}| baz}|" (foo "bar " (x "@") " baz")
     "@foo|--{bar}@|{baz}--|" (foo "bar}@|{baz")
     "@foo|<<{bar}@|{baz}>>|" (foo "bar}@|{baz")
     "(define \\@email \"foo@bar.com\")" (define \@email "foo@bar.com")
     ;;"(define |@atchar| #\\@)" (define \@atchar #\@)
     "@foo{bar @baz[2 3] {4 5}}" (foo "bar " (baz 2 3) " {4 5}")
     ;;"@`',@foo{blah}" `',@(foo "blah")
     ;;"@#`#'#,@foo{blah}"  #`#'#,@(foo "blah")
     "@(lambda (x) x){blah}" ((lambda (x) x) "blah")
     ;;"@`(unquote foo){blah}" `(,foo  "blah")
     "@{foo bar
        baz}" ("foo bar" *lf* "baz")
     "@'{foo bar
         baz}" '("foo bar" *lf* "baz")
     "@foo{bar @; comment
           baz@;
           blah}" (foo "bar bazblah")
     "@foo{x @y z}" (foo "x " y " z")
     "@foo{x @(* y 2) z}" (foo "x " (* y 2) " z")
     "@{@foo bar}" (foo " bar")
     "@@foo{bar}{baz}" ((foo "bar") "baz")
     "@foo[1 (* 2 3)]{bar}" (foo 1 (* 2 3) "bar")
     "@foo[@bar{...}]{blah}" (foo (bar "...") "blah")
     "@foo[bar]" (foo bar)
     "@foo{bar @f[x] baz}" (foo "bar " (f x) " baz")
     "@foo[]{bar}" (foo "bar")
     "@foo[]" (foo)
     "@foo" foo
     "@foo{}" (foo)
     "@foo[:style 'big]{bar}" (foo :style 'big "bar") ; #:style in racket
     "@foo{f{o}o}" (foo "f{o}o")
     "@foo{{{}}{}}" (foo "{{}}{}")
     "@foo{bar}" (foo "bar")
     "@foo{ bar }" (foo " bar ")
     "@foo[1]{ bar }" (foo 1 " bar ")
     "@foo{a @bar{b} c}" (foo "a " (bar "b") " c")
     "@foo{a @bar c}" (foo "a " bar " c")
     "@foo{a @(bar 2) c}" (foo "a " (bar 2) " c")
     "@foo{A @\"}\" marks the end}" (foo "A } marks the end")
     "@foo{The prefix: @\"@\".}" (foo "The prefix: @.")
     "@foo{@\"@x{y}\" --> (x \"y\")}" (foo "@x{y} --> (x \"y\")")
     "@foo|{...}|" (foo "...")
     "@foo|{\"}\" follows \"{\"}|" (foo "\"}\" follows \"{\"")
     "@foo|{Nesting |{is}| ok}|" (foo "Nesting |{is}| ok")
     "@foo|{Maze
            |@bar{is}
            Life!}|" (foo "Maze" *lf*
                          (bar "is") *lf*
                          "Life!")
     "@t|{In |@i|{sub|@\"@\"s}| too}|" (t "In " (i "sub@s") " too")
     "@foo|<<<{@x{foo} |@{bar}|.}>>>|" (foo "@x{foo} |@{bar}|.")
     "@foo|!!{X |!!@b{Y}...}!!|" (foo "X " (b "Y") "...")
     "@foo{foo@bar.}" (foo "foo" bar.)
     "@foo{foo@|bar|.}" (foo "foo" bar ".")
     "@foo{foo@3.0}" (foo "foo" 3.0) ;; orig had 3. 3.0
     "@foo{foo@|3|.0}" (foo "foo" 3 ".0") ;; orign had no 0
     "@foo{foo@|(f 1)|{bar}}" (foo "foo" (f 1) "{bar}")
     "@foo{foo@|bar|[1]{baz}}" (foo "foo" bar "[1]{baz}")
     "@foo{x@\"y\"z}" (foo "xyz")
     "@foo{x@|\"y\"|z}" (foo "x" "y" "z")
     "@foo{x@|1 (+ 2 3) 4|y}" (foo "x" 1 (+ 2 3) 4 "y")
     "@foo{x@|*
              *|y}" (foo "x" *
          * "y")
     "@foo{Alice@||Bob@|
           |Carol}" (foo "Alice" "Bob" "Carol")
     "@|{blah}|" ("blah")
     "@|{blah |@foo bleh}|" ("blah " foo " bleh")
     "@foo{First line@;{there is still a
                        newline here;}
           Second line}" (foo "First line" *lf* "Second line")
     "@foo{A long @;
           single-@;
           string arg.}" (foo "A long single-string arg.")
     "@foo{bar}" (foo "bar")
     "@foo{ bar }" (foo " bar ")
     "@foo{ bar
           baz }" (foo " bar" *lf* "baz ")
     "@foo{bar
      }" (foo "bar")
     "@foo{
      bar
      }" (foo "bar")
     "@foo{

      bar

      }" (foo *lf* "bar" *lf*)
      "@foo{
      bar

      baz
      }" (foo "bar" *lf* *lf* "baz")
     "@foo{
      }" (foo *lf*)
     "@foo{

      }" (foo *lf* *lf*)
     "@foo{ bar
      baz }" (foo " bar" *lf* "baz ")
     "@foo{
        bar
        baz
        blah
      }" (foo "bar" *lf* "baz" *lf* "blah")
     "@foo{
      begin
        x++;
      end}" (foo "begin" *lf* "  " "x++;" *lf* "end")
     "@foo{
         a
        b
       c}" (foo "  " "a" *lf* " " "b" *lf* "c")
     "@foo{bar
             baz
           bbb}" (foo "bar" *lf* "  ""baz" *lf* "bbb")
     "@foo{ bar
              baz
            bbb}" (foo " bar" *lf* "   " "baz" *lf* " " "bbb")
     "@foo{bar
         baz
         bbb}" (foo "bar" *lf* "baz" *lf* "bbb")
     "@foo{ bar
           baz
           bbb}" (foo " bar" *lf* "baz" *lf* "bbb")
     "@foo{ bar
         baz
           bbb}" (foo " bar" *lf* "baz" *lf* "  " "bbb")
     "@text{Some @b{bold
                    text}, and
            more text.}" (text "Some " (b "bold" *lf* "text")", and" *lf* "more text.")
#| ;;; properly render this?
;;; a formatter will need to apply the 2-space indentation to the rendering of the bold body.
@code{
  begin
    i = 1, r = 1
    @bold{while i < n do
            r *= i++
          done}
  end
}
|#
     "@foo{
        @|| bar @||
        @|| baz}" (foo " bar " *lf* " baz")
)))

(deftest test-compile-file ()
  (with-temporary-file (:stream s :pathname p :type "lisp" :direction :output)
    (princ "
 (in-package :scribble/test)
 (in-readtable :scribble)
 (defun compiled-foo ()
   (list [foo ,(+ 1 2)]
         @'foo[bar]{baz @(quux) toto}
         '@this{
  is
    indented
      from the 'is'.
  }))
" s)
    :close-stream
    (ensure-directories-exist (compile-file-pathname* p))
    (load (compile-file* p))
    (is (equal (funcall 'compiled-foo)
               `(("foo " 3)
                 (foo bar "baz " (quux) " toto")
                 (this "is" ,*lf* "  " "indented" ,*lf* "    " "from the 'is'.")))))
  t)
