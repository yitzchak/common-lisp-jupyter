(defpackage #:fredokun-utilities
  (:nicknames #:fredo-utils)
  (:use #:cl)
  (:export #:*example-enabled*
           #:*example-equal-predicate*
           #:example
           #:example-progn
           #:info
           #:vbinds
           #:add-terminator
           #:starts-with-p
           #:ends-with-p
           #:ends-with-terminator
           #:file-to-base64-string
           #:read-file-lines
           #:read-string-file
           #:read-binary-file))

(defpackage #:maxima-jupyter
  (:use #:cl #:fredo-utils #:iterate)
  (:export
    #:kernel-start
    #:kernel-start-exec
    #:make-error-result
    #:make-file-result
    #:make-inline-result
    #:make-lisp-result
    #:make-maxima-result))

(in-package #:maxima-jupyter)
