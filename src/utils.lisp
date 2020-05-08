(in-package #:jupyter)

#|

# CommonTypes: Utilities #

|#

; (defvar maxima::$kernel_info nil)

(defparameter +uuid-size+ 16)

(defun octets-to-hex-string (bytes)
  (format nil "~(~{~2,'0X~}~)" (coerce bytes 'list)))

(defun make-uuid (&optional as-bytes)
  (let ((bytes (make-array +uuid-size+ :element-type '(unsigned-byte 8))))
    (dotimes (index +uuid-size+)
      (setf (aref bytes index)
        (if (= 6 index)
          (logior #x40 (random 16))
          (random 256))))
    (if as-bytes
      bytes
      (octets-to-hex-string bytes))))

(defun json-getf (object indicator &optional default)
  "Safe accessor for the internal JSON format that behaves like getf"
  (if-let ((pair (assoc indicator (cdr object) :test #'string=)))
    (cdr pair)
    default))

(defun read-raw-string (stream c1 c2)
  (declare (ignore c1 c2))
  (iter
    (for ch next (read-char stream))
    (until (char= ch #\"))
    (collect ch result-type 'string)))

(set-dispatch-macro-character #\# #\" #'read-raw-string)
