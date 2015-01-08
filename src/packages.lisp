
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
  (:export parse-json
	   parse-json-from-string
	   encode-json
	   encode-json-to-string))

(defpackage #:fishbowl
  (:use #:cl #:fredo-utils #:myjson)
  (:export kernel-start))

(defpackage #:fishbowl-user
  (:use #:cl #:common-lisp-user)
  (:export quit))

(in-package #:fishbowl)
