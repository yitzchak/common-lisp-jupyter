
(defpackage #:common-lisp-utilities
  (:nicknames #:cl-utilities #:cl-utils)
  (:use #:cl)
  (:export *example-enabled*
           *example-equal-predicate*
           example
           example-progn
           *logg-enabled*
           *logg-level*
           logg
           vbinds
           afetch
	   while
	   read-file-lines))

(defpackage #:uncommonshell
  (:use #:cl #:cl-utilities)
  (:export kernel-start))

(in-package #:uncommonshell)
