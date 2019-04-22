(in-package #:jupyter)

#|

# CommonTypes: Utilities #

|#

; (defvar maxima::$kernel_info nil)

(defun make-uuid ()
  (remove #\- (format nil "~(~A~)" (uuid:make-v4-uuid))))

(defun json-getf (object indicator &optional default)
  "Safe accessor for the internal JSON format that behaves like getf"
  (iter
    (for (key . value) in (cdr object))
    (when (string= indicator key)
      (leave value))
    (finally
      (return default))))

(defun read-raw-string (stream c1 c2)
  (declare (ignore c1 c2))
  (iter
    (for ch next (read-char stream))
    (until (char= ch #\"))
    (collect ch result-type 'string)))

(set-dispatch-macro-character #\# #\" #'read-raw-string)
