(defpackage #:jupyter
  (:use #:cl #:iterate)
  (:export
    #:file
    #:gif-file
    #:jpeg-file
    #:pdf-file
    #:png-file
    #:ps-file
    #:svg-file
    #:inline
    #:text
    #:html
    #:jpeg
    #:latex
    #:markdown
    #:png
    #:svg
    #:enqueue-input
    #:evaluate
    #:handling-errors
    #:info
    #:install-kernel
    #:is-complete
    #:kernel
    #:kernel-start
    #:kernel-start-exec
    #:make-error-result
    #:make-eval-error
    #:make-file-result
    #:make-inline-result
    #:make-lisp-result
    #:quit
    #:quit-eval-error-p
    #:render
    #:result
    #:send-result))

(defpackage #:cl-jupyter
  (:use #:cl #:iterate)
  (:export
    #:kernel))

(in-package #:jupyter)
