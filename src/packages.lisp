(defpackage #:jupyter
  (:use #:cl #:iterate)
  (:export
    #:*page-output*
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
    #:is-complete
    #:info
    #:install-kernel
    #:kernel
    #:run-kernel
    #:kernel-prompt-prefix
    #:kernel-prompt-suffix
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
