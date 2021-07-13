(in-package #:jupyter/common-lisp)


(defclass fragment ()
  ((parent
     :accessor fragment-parent
     :initarg :parent
     :initform nil)
   (position
     :accessor fragment-position
     :initarg :position
     :initform nil)
   (value
     :accessor fragment-value
     :initarg :value
     :initform nil)
   (status
     :accessor fragment-status
     :initarg :status
     :initform nil)
   (children
     :accessor fragment-children
     :initarg :children
     :initform nil)
   (start
     :accessor fragment-start
     :initarg :start)
   (end
     :accessor fragment-end
     :initarg :end)))


(defclass symbol-name-fragment (fragment)
  ())


(defclass package-name-fragment (fragment)
  ())


(defclass package-marker-fragment (fragment)
  ())


(defclass symbol-fragment (fragment)
  ())


(defclass skipped-fragment (fragment)
  ())


(defmethod initialize-instance :after ((instance fragment) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (trivial-do:dolist* (position child (fragment-children instance))
    (setf (fragment-position child) position)
    (setf (fragment-parent child) instance)))


(defclass my-client (eclector.parse-result:parse-result-client)
  ())


(defmethod eclector.reader:interpret-symbol-token ((client my-client) input-stream
                                   token
                                   position-package-marker-1
                                   position-package-marker-2)
  (cond
    (position-package-marker-2
      (make-instance 'symbol-fragment
                     :status :internal
                     :start 0
                     :end (length token)
                     :children (list (make-instance 'package-name-fragment
                                                    :value (subseq token 0 position-package-marker-1)
                                                    :start 0
                                                    :end position-package-marker-1)
                                     (make-instance 'package-marker-fragment
                                                    :value (subseq token position-package-marker-1 (1+ position-package-marker-2))
                                                    :start  (1+ position-package-marker-2)
                                                    :end (length token))
                                     (make-instance 'symbol-name-fragment
                                                    :value (subseq token (1+ position-package-marker-2))
                                                    :start (1+ position-package-marker-2)
                                                    :end (length token)))))
    (position-package-marker-1
      (make-instance 'symbol-fragment
                     :status :external
                     :start 0
                     :end (length token)
                     :children (list (make-instance 'package-name-fragment
                                                    :value (subseq token 0 position-package-marker-1)
                                                    :start 0
                                                    :end position-package-marker-1)
                                     (make-instance 'package-marker-fragment
                                                    :value (subseq token position-package-marker-1 (1+ position-package-marker-1))
                                                    :start position-package-marker-1
                                                    :end (1+ position-package-marker-1))
                                     (make-instance 'symbol-name-fragment
                                                    :value (subseq token (1+ position-package-marker-1))
                                                    :start (1+ position-package-marker-1)
                                                    :end (length token)))))
    (t
      (make-instance 'symbol-fragment
                     :status :local
                     :start 0
                     :end (length token)
                     :children (list (make-instance 'symbol-name-fragment
                                                    :value token
                                                    :start 0
                                                    :end (length token)))))))


(defmethod eclector.parse-result:make-expression-result ((client my-client) result children source)
  (make-instance 'fragment :value result :start (car source) :end (cdr source) :children children))


(defun fragment-shift (frag offset)
  (incf (fragment-start frag) offset)
  (incf (fragment-end frag) offset)
  (dolist (child (fragment-children frag))
    (fragment-shift child offset)))


(defmethod eclector.parse-result:make-expression-result ((client my-client) (result fragment) children source)
  (fragment-shift result (car source))
  result)


(defmethod eclector.parse-result:make-skipped-input-result ((client my-client) stream reason source)
  (declare (ignore stream))
  (make-instance 'skipped-fragment :status reason :start (car source) :end (cdr source)))


(defun read-fragment (stream)
  (ignore-errors
    (handler-bind
        ((error #'eclector.base:recover))
      (eclector.parse-result:read (make-instance 'my-client) stream nil))))


(defun find-fragment (frag pos)
  (cond
    ((or (< pos (fragment-start frag))
         (>= pos (fragment-end frag)))
      (values nil nil))
    ((null (fragment-children frag))
      (values frag t))
    (t
      (dolist (child (fragment-children frag) (values frag t))
        (let ((fc (find-fragment child pos)))
          (when fc
            (return (values fc t))))))))


(defun locate-fragment (code pos)
  (with-input-from-string (stream code)
    (do ((frag (read-fragment stream) (read-fragment stream)))
        ((null frag))
      (multiple-value-bind (enclosing-frag contained)
                           (find-fragment frag pos)
        (when contained
          (return enclosing-frag))))))
