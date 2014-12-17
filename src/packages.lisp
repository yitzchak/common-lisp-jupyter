
(defpackage #:common-lisp-utilities
  (:nicknames #:cl-utilities #:cl-utils)
  (:use #:cl)
  (:export *example-enabled*
           *example-equal-predicate*
           example
           *logg-enabled*
           *logg-level*
           logg
           vbinds))

(defpackage #:uncommonshell
  (:use #:cl #:cl-utilities)
  (:export))

(in-package #:uncommonshell)
