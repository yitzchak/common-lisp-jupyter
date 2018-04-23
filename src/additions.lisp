(in-package #:maxima)

#|

This is the entry point for starting the kernel from within an existing Maxima
session.

|#

(defmfun $jupyter_kernel_start (connection-file-name)
  (maxima-jupyter::kernel-start connection-file-name))

#|

Convenience functions to return specific types from Lisp or Maxima.

|#

(defun jupyter-file (path &optional (display nil))
  (maxima-jupyter::make-file-result path :display display :handle t))

(defmfun $jupyter_file (path &optional (display nil))
  (maxima-jupyter::make-file-result path :display display :handle t))

(defun jupyter-inline (value mime-type &optional (display nil))
  (maxima-jupyter::make-inline-result value :mime-type mime-type
                                            :display display
                                            :handle t))

(defmfun $jupyter_inline (value mime-type &optional (display nil))
  (maxima-jupyter::make-inline-result value :mime-type mime-type
                                            :display display
                                            :handle t))

(defun jupyter-text (value &optional (display nil))
  (maxima-jupyter::make-inline-result value :display display :handle t))

(defmfun $jupyter_text (value &optional (display nil))
  (maxima-jupyter::make-inline-result value :display display :handle t))

(defun jupyter-html (value &optional (display nil))
  (maxima-jupyter::make-inline-result value :mime-type maxima-jupyter::*html-mime-type*
                                            :display display
                                            :handle t))

(defmfun $jupyter_html (value &optional (display nil))
  (maxima-jupyter::make-inline-result value :mime-type maxima-jupyter::*html-mime-type*
                                            :display display
                                            :handle t))

(defun jupyter-jpeg (value &optional (display nil))
  (maxima-jupyter::make-inline-result value :mime-type maxima-jupyter::*jpeg-mime-type*
                                            :display display
                                            :handle t))

(defmfun $jupyter_jpeg (value &optional (display nil))
  (maxima-jupyter::make-inline-result value :mime-type maxima-jupyter::*jpeg-mime-type*
                                            :display display
                                            :handle t))

(defun jupyter-latex (value &optional (display nil))
  (maxima-jupyter::make-inline-result value :mime-type maxima-jupyter::*latex-mime-type*
                                            :display display
                                            :handle t))

(defmfun $jupyter_latex (value &optional (display nil))
  (maxima-jupyter::make-inline-result value :mime-type maxima-jupyter::*latex-mime-type*
                                            :display display
                                            :handle t))

(defun jupyter-markdown (value &optional (display nil))
  (maxima-jupyter::make-inline-result value :mime-type maxima-jupyter::*markdown-mime-type*
                                            :display display
                                            :handle t))

(defmfun $jupyter_markdown (value &optional (display nil))
  (maxima-jupyter::make-inline-result value :mime-type maxima-jupyter::*markdown-mime-type*
                                            :display display
                                            :handle t))

(defun jupyter-png (value &optional (display nil))
  (maxima-jupyter::make-inline-result value :mime-type maxima-jupyter::*png-mime-type*
                                            :display display
                                            :handle t))

(defmfun $jupyter_png (value &optional (display nil))
  (maxima-jupyter::make-inline-result value :mime-type maxima-jupyter::*png-mime-type*
                                            :display display
                                            :handle t))

(defun jupyter-svg (value &optional (display nil))
  (maxima-jupyter::make-inline-result value :mime-type maxima-jupyter::*svg-mime-type*
                                            :display display
                                            :handle t))

(defmfun $jupyter_svg (value &optional (display nil))
  (maxima-jupyter::make-inline-result value :mime-type maxima-jupyter::*svg-mime-type*
                                            :display display
                                            :handle t))
