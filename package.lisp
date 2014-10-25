;;;; package.lisp

(defpackage #:cg-common-ground
  (:use #:cl #:iterate)
  (:export #:joinl #:join #:frnl #:fart
	   #:camelcaseize #:underscorize #:capitalize
	   #:camcase-if-not-string #:unscore-if-not-string #:*symbol-stringification-style
	   #:stringify-symbol #:stringify-if-symbol
	   #:parse-out-keywords))
