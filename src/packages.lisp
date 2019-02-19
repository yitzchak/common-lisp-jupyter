(defpackage #:jupyter
  (:use #:cl #:iterate)
  (:export
    ; utils
    #:info
    #:install-kernel
    ; results
    #:file
    #:gif-file
    #:html
    #:inline-result
    #:jpeg
    #:jpeg-file
    #:latex
    #:make-error-result
    #:make-file-result
    #:make-inline-result
    #:make-lisp-result
    #:markdown
    #:pdf-file
    #:png
    #:png-file
    #:ps-file
    #:quit-eval-error-p
    #:render
    #:result
    #:svg
    #:svg-file
    #:text
    ; kernel
    #:*page-output*
    #:clear
    #:code-is-complete
    #:comm
    #:comm-id
    #:complete-code
    #:create-comm
    #:enqueue-input
    #:evaluate-code
    #:handling-errors
    #:inspect-code
    #:kernel
    #:kernel-prompt-prefix
    #:kernel-prompt-suffix
    #:on-comm-close
    #:on-comm-message
    #:on-comm-open
    #:quit-condition
    #:run-kernel
    #:send-comm-close
    #:send-comm-message
    #:send-comm-open
    #:send-result))

(defpackage #:jupyter-widgets
  (:use #:cl #:iterate)
  (:export
    #:button-description
    #:dom-widget
    #:make-button
    #:on-button-click
    #:widget))

(defpackage #:common-lisp-jupyter
  (:nicknames :cl-jupyter)
  (:use #:cl #:iterate)
  (:export
    #:kernel))

(in-package #:jupyter)
