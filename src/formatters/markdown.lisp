(in-package #:jupyter/markdown-formatter)


(defparameter *indent-level* 0)


(defun indent (stream)
  (dotimes (indent (* 4 *indent-level*))
    (write-char #\space stream)))


(defun write-escaped-string (text &optional stream)
  (dotimes (pos (length text))
    (when (position (char text pos) "\\`*_{}[]#+-!")
      (write-char #\\ stream))
    (write-char (char text pos) stream)))


(defun code (stream value colon at &rest args)
  (declare (ignore args))
  (indent stream)
  (write-line "```lisp" stream)
  (indent stream)
  (if colon
    (write-string value stream)
    (write value :escape (not at) :case :downcase :pretty t :stream stream))
  (terpri stream)
  (indent stream))


(defun text (stream value colon at &rest args)
  (declare (ignore args))
  (write-escaped-string
    (if colon
       value
       (write-to-string value :escape (not at) :case :downcase))
    stream))
