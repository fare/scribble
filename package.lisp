(cl:defpackage #:scribble
  (:use #:meta #:common-lisp)
  (:export #:enable-scribble-syntax #:disable-scribble-syntax
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
	   #:configure-scribble-for-yaclml))
