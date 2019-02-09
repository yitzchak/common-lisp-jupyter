(in-package #:maxima)

#|

This is the entry point for starting the kernel from within an existing Maxima
session.

|#

(defmfun $jupyter_kernel_start (connection-file-name)
  (cl-jupyter-kernel::kernel-start connection-file-name))

#|

Convenience functions to return specific types from Lisp or Maxima.

|#

(defun jupyter-file (path &optional (display nil))
  (cl-jupyter-kernel::make-file-result path :display display :handle t))

(defmfun $jupyter_file (path &optional (display nil))
  (cl-jupyter-kernel::make-file-result path :display display :handle t))

(defun jupyter-gif-file (path &optional (display nil))
  (cl-jupyter-kernel::make-file-result path
                                    :display display :handle t
                                    :mime-type cl-jupyter-kernel::*gif-mime-type*))

(defmfun $jupyter_gif_file (path &optional (display nil))
  (cl-jupyter-kernel::make-file-result path
                                    :display display :handle t
                                    :mime-type cl-jupyter-kernel::*gif-mime-type*))

(defun jupyter-jpeg-file (path &optional (display nil))
  (cl-jupyter-kernel::make-file-result path
                                    :display display :handle t
                                    :mime-type cl-jupyter-kernel::*jpeg-mime-type*))

(defmfun $jupyter_jpeg_file (path &optional (display nil))
  (cl-jupyter-kernel::make-file-result path
                                    :display display :handle t
                                    :mime-type cl-jupyter-kernel::*jpeg-mime-type*))

(defun jupyter-pdf-file (path &optional (display nil))
  (cl-jupyter-kernel::make-file-result path
                                    :display display :handle t
                                    :mime-type cl-jupyter-kernel::*pdf-mime-type*))

(defmfun $jupyter_pdf_file (path &optional (display nil))
  (cl-jupyter-kernel::make-file-result path :display display :handle t
                                    :mime-type cl-jupyter-kernel::*pdf-mime-type*))

(defun jupyter-png-file (path &optional (display nil))
  (cl-jupyter-kernel::make-file-result path :display display :handle t
                                         :mime-type cl-jupyter-kernel::*png-mime-type*))

(defmfun $jupyter_png_file (path &optional (display nil))
  (cl-jupyter-kernel::make-file-result path :display display :handle t
                                         :mime-type cl-jupyter-kernel::*png-mime-type*))

(defun jupyter-ps-file (path &optional (display nil))
  (cl-jupyter-kernel::make-file-result path
                                    :display display :handle t
                                    :mime-type cl-jupyter-kernel::*ps-mime-type*))

(defmfun $jupyter_ps_file (path &optional (display nil))
  (cl-jupyter-kernel::make-file-result path :display display :handle t
                                    :mime-type cl-jupyter-kernel::*ps-mime-type*))

(defun jupyter-svg-file (path &optional (display nil))
  (cl-jupyter-kernel::make-file-result path :display display :handle t
                                         :mime-type cl-jupyter-kernel::*svg-mime-type*))

(defmfun $jupyter_svg_file (path &optional (display nil))
  (cl-jupyter-kernel::make-file-result path :display display :handle t
                                         :mime-type cl-jupyter-kernel::*svg-mime-type*))

(defun jupyter-inline (value mime-type &optional (display nil))
  (cl-jupyter-kernel::make-inline-result value :mime-type mime-type
                                            :display display
                                            :handle t))

(defmfun $jupyter_inline (value mime-type &optional (display nil))
  (cl-jupyter-kernel::make-inline-result value :mime-type mime-type
                                            :display display
                                            :handle t))

(defun jupyter-text (value &optional (display nil))
  (cl-jupyter-kernel::make-inline-result value :display display :handle t))

(defmfun $jupyter_text (value &optional (display nil))
  (cl-jupyter-kernel::make-inline-result value :display display :handle t))

(defun jupyter-html (value &optional (display nil))
  (cl-jupyter-kernel::make-inline-result value :mime-type cl-jupyter-kernel::*html-mime-type*
                                            :display display
                                            :handle t))

(defmfun $jupyter_html (value &optional (display nil))
  (cl-jupyter-kernel::make-inline-result value :mime-type cl-jupyter-kernel::*html-mime-type*
                                            :display display
                                            :handle t))

(defun jupyter-jpeg (value &optional (display nil))
  (cl-jupyter-kernel::make-inline-result value :mime-type cl-jupyter-kernel::*jpeg-mime-type*
                                            :display display
                                            :handle t))

(defmfun $jupyter_jpeg (value &optional (display nil))
  (cl-jupyter-kernel::make-inline-result value :mime-type cl-jupyter-kernel::*jpeg-mime-type*
                                            :display display
                                            :handle t))

(defun jupyter-latex (value &optional (display nil))
  (cl-jupyter-kernel::make-inline-result value :mime-type cl-jupyter-kernel::*latex-mime-type*
                                            :display display
                                            :handle t))

(defmfun $jupyter_latex (value &optional (display nil))
  (cl-jupyter-kernel::make-inline-result value :mime-type cl-jupyter-kernel::*latex-mime-type*
                                            :display display
                                            :handle t))

(defun jupyter-markdown (value &optional (display nil))
  (cl-jupyter-kernel::make-inline-result value :mime-type cl-jupyter-kernel::*markdown-mime-type*
                                            :display display
                                            :handle t))

(defmfun $jupyter_markdown (value &optional (display nil))
  (cl-jupyter-kernel::make-inline-result value :mime-type cl-jupyter-kernel::*markdown-mime-type*
                                            :display display
                                            :handle t))

(defun jupyter-png (value &optional (display nil))
  (cl-jupyter-kernel::make-inline-result value :mime-type cl-jupyter-kernel::*png-mime-type*
                                            :display display
                                            :handle t))

(defmfun $jupyter_png (value &optional (display nil))
  (cl-jupyter-kernel::make-inline-result value :mime-type cl-jupyter-kernel::*png-mime-type*
                                            :display display
                                            :handle t))

(defun jupyter-svg (value &optional (display nil))
  (cl-jupyter-kernel::make-inline-result value :mime-type cl-jupyter-kernel::*svg-mime-type*
                                            :display display
                                            :handle t))

(defmfun $jupyter_svg (value &optional (display nil))
  (cl-jupyter-kernel::make-inline-result value :mime-type cl-jupyter-kernel::*svg-mime-type*
                                            :display display
                                            :handle t))
