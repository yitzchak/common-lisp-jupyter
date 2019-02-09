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

(defpackage #:jupyter-kernel
  (:use #:cl #:fredo-utils #:iterate)
  (:export
    #:kernel
    #:kernel-start
    #:kernel-start-exec
    #:is-complete
    #:evaluate
    #:quit-eval-error-p
    #:handling-errors
    #:make-error-result
    #:make-file-result
    #:make-inline-result
    #:make-lisp-result))

(defpackage #:cl-jupyter
  (:use #:cl #:iterate)
  (:export
    #:kernel))

(in-package #:jupyter-kernel)
