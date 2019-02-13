(in-package #:jupyter)

#|

# CommonTypes: Utilities #

|#

; (defvar maxima::$kernel_info nil)

(defun info (&rest args)
  (when nil;maxima::$kernel_info
    (apply #'format *trace-output* args)))

(defun read-file-lines (filename)
  (with-open-file (input filename)
    (loop
       for line = (read-line input nil 'eof)
       until (eq line 'eof)
       collect line)))

(defun read-binary-file (filename)
  (with-open-file (stream filename :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence bytes stream)
      bytes)))

(defun read-string-file (filename)
  (with-open-file (stream filename)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defun file-to-base64-string (path)
  (cl-base64:usb8-array-to-base64-string (read-binary-file path)))

;; nicked from: https://rosettacode.org/wiki/String_matching#Common_Lisp
(defun starts-with-p (str1 str2)
  (let ((p (search str2 str1)))
    (and p (= 0 p))))

;; nicked from: https://rosettacode.org/wiki/String_matching#Common_Lisp
(defun ends-with-p (str1 str2)
  (let ((p (mismatch str2 str1 :from-end T)))
    (or (not p) (= 0 p))))

(defun install-kernel (argv name language)
  (let ((kernel-path (merge-pathnames
                       (make-pathname :directory (list :relative
                                                       ; Just in case HFS+ is
                                                       ; case-sensitive
                                                       (if (uiop:os-macosx-p)
                                                         "Jupyter"
                                                         "jupyter")
                                                       "kernels"
                                                       language)
                                      :name "kernel"
                                      :type "json")
                       (if (uiop:os-macosx-p)
                         (merge-pathnames
                           (make-pathname :directory '(:relative "Library"))
                           (uiop:getenv-pathname "HOME" :ensure-directory t))
                         (uiop:xdg-data-home)))))
  (format t "Installing kernel spec file ~A~%" kernel-path)
  (ensure-directories-exist kernel-path)
  (with-open-file (stream kernel-path :direction :output :if-exists :supersede)
    (write-string
      (jsown:to-json
        (jsown:new-js
          ("argv" argv)
          ("display_name" name)
          ("language" language)))
      stream))))
