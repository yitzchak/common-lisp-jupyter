(in-package #:jupyter)

#|

# CommonTypes: Utilities #

|#

; (defvar maxima::$kernel_info nil)

(defparameter +uuid-size+ 16)
(defparameter *uuid-random-state* (make-random-state t))

(defun octets-to-hex-string (bytes)
  (format nil "~(~{~2,'0X~}~)" (coerce bytes 'list)))

(defun make-uuid (&optional as-bytes)
  (let ((bytes (make-array +uuid-size+ :element-type '(unsigned-byte 8))))
    (dotimes (index +uuid-size+)
      (setf (aref bytes index)
        (if (= 6 index)
          (logior #x40 (random 16 *uuid-random-state*))
          (random 256 *uuid-random-state*))))
    (if as-bytes
      bytes
      (octets-to-hex-string bytes))))

(defun json-getf (object indicator &optional default)
  "Safe accessor for the internal JSON format that behaves like getf"
  (if-let ((pair (assoc indicator (cdr object) :test #'string=)))
    (cdr pair)
    default))

(defmethod (setf json-getf) (new-value object indicator &optional default)
  (declare (ignore default))
  (if-let ((pair (assoc indicator (cdr object) :test #'string=)))
    (rplacd pair new-value)
    (rplacd object (acons indicator new-value (cdr object)))))

(defmacro json-extend-obj (object &body specs)
  (with-gensyms (obj-var)
    `(let ((,obj-var ,object))
       ,@(mapcar (lambda (spec)
                   `(setf (gethash ,(first spec) ,obj-var) (progn ,@(rest spec))))
                 specs)
       ,obj-var)))

(defun json-empty-obj ()
  (make-hash-table :test #'equal))

(defmacro json-new-obj (&body specs)
  `(json-extend-obj (json-empty-obj)
     ,@specs))

(defun json-keyp (object indicator)
  (nth-value 1 (gethash indicator object)))

(defun read-raw-string (stream c1 c2)
  (declare (ignore c1 c2))
  (iter
    (for ch next (read-char stream))
    (until (char= ch #\"))
    (collect ch result-type 'string)))

(set-dispatch-macro-character #\# #\" #'read-raw-string)

(defun symbol-to-camel-case (s)
  (do ((name (symbol-name s))
       (position 0 (1+ position))
       (result "")
       capitalize)
      ((= position (length name)) result)
    (cond
      ((char= (char name position) #\-)
        (setq capitalize t))
      (capitalize
        (setq result (concatenate 'string result (string (char-upcase (char name position)))))
        (setq capitalize nil))
      (t
        (setq result (concatenate 'string result (string (char-downcase (char name position)))))))))

(defun camel-case-to-symbol (name)
  (intern
    (do ((position 0 (1+ position))
         (result ""))
        ((= position (length name)) result)
      (when (and (not (zerop position))
                 (upper-case-p (char name position)))
        (setq result (concatenate 'string result "-")))
        (setq result (concatenate 'string result (string (char-upcase (char name position))))))
    "KEYWORD"))

(defun symbol-to-snake-case (s)
  (substitute #\_ #\%
    (substitute #\_ #\-
      (string-downcase (symbol-name s)))))

(defun snake-case-to-symbol (k)
  (intern
    (string-upcase
      (substitute #\- #\_
                  (if (and (not (zerop (length k)))
                                (char= (char k 0) #\_))
                    (substitute #\% #\_ k :count 1)
                    k)))
    "KEYWORD"))

(defun json-to-plist (value &key symbol-case)
  (mapcan (lambda (pair)
            (list (case symbol-case
                    (:snake
                      (snake-case-to-symbol (car pair)))
                    (:camel
                      (camel-case-to-symbol (car pair)))
                    (otherwise
                      (intern (car pair) "KEYWORD")))
                  (cdr pair)))
          (cdr value)))

(defun json-to-nested-plist (value &key symbol-case)
  (if (and (listp value)
           (eql :obj (car value)))
    (mapcan (lambda (pair)
              (list (case symbol-case
                      (:snake
                        (snake-case-to-symbol (car pair)))
                      (:camel
                        (camel-case-to-symbol (car pair)))
                      (otherwise
                        (intern (car pair) "KEYWORD")))
                    (json-to-nested-plist (cdr pair) :symbol-case symbol-case)))
            (cdr value))
    value))

