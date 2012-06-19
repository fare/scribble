#+xcvb (module ())

(cl:defpackage #:scribble
  (:use #:common-lisp #:meta :fare-utils :fare-quasiquote :named-readtables)
  #+(or clisp sbcl ccl)
  (:import-from #+clisp :gray #+sbcl :sb-gray #+ccl :ccl
                :stream-line-column)
  (:export #:enable-scribble-at-syntax #:disable-scribble-at-syntax
	   #:reenable-scribble-at-syntax
           #:enable-scribble-syntax #:disable-scribble-syntax
	   #:reenable-scribble-syntax
	   #:enable-sub-scribble-syntax #:disable-sub-scribble-syntax
	   #:reenable-sub-scribble-syntax
	   #:*scribble-preprocessor* #:*scribble-preprocess* #:pp #:with-preprocessor
	   #:*scribble-list*
	   #:*scribble-default-head* #:default-scribble-list
	   #:*scribble-package* #:*scribble-cons* #:default-scribble-cons
	   #:configure-scribble
	   #:configure-scribble-for-exscribe
	   #:configure-scribble-for-araneida
	   #:configure-scribble-for-htmlgen
	   #:configure-scribble-for-lml2
	   #:configure-scribble-for-tml
	   #:configure-scribble-for-who
	   #:configure-scribble-for-yaclml

           #:string-column-modifier
           #:combine-column-modifiers
           #:stream-line-column-harder
           #:read-stream-to-pos

           
           ))
