
(defpackage #:fredokun-utilities
  (:nicknames #:fredo-utils)
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

(defpackage #:myjson
  (:use #:cl #:fredo-utils)
  (:export))

(defpackage #:uncommonshell
  (:use #:cl #:fredo-utils)
  (:export kernel-start))

(in-package #:uncommonshell)
