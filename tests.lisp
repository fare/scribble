(defpackage :scribble-test
  (:use :common-lisp :scribble :hu.dwim.stefil :fare-utils))

(in-package :scribble-test)

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
    (scm "fooabcde" nil 5 nil)
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


(defparameter *u* "/tmp/u")

(defparameter *external-format*
  #-ccl :default
  #+ccl (ccl:make-external-format :character-encoding :utf-8))

(deftest test-file-position ()
  (with-open-file (s *u* :direction :output :if-exists :rename-and-delete)
    (princ "Hello, World
Faré λ 自由 foo
æéïôù
" s))

  (with-open-file (s *u* :direction :input :external-format *external-format*)
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
        )))

  (delete-file *u*)
  t)
