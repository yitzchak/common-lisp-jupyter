(in-package #:common-lisp-jupyter)


(defclass fragment ()
  ((parent
     :accessor fragment-parent
     :initarg :parent
     :initform nil)
   (position
     :accessor fragment-position
     :initarg :position
     :initform nil)
   (result
     :accessor fragment-result
     :initarg :result
     :initform nil)
   (types
     :accessor fragment-types
     :initarg :types
     :initform (list :expr))
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


(defun make-fragment (&rest initargs)
  (apply #'make-instance 'fragment initargs))


(defclass my-client (eclector.parse-result:parse-result-client)
  ())


(defmethod eclector.reader:interpret-symbol-token ((client my-client) input-stream
                                   token
                                   position-package-marker-1
                                   position-package-marker-2)
  (cond
    (position-package-marker-2
      (let ((parent (make-fragment :types (list :internal-symbol)
                                   :start 0
                                   :end (length token))))
        (setf (fragment-children parent)
              (list (make-fragment :result (subseq token 0 position-package-marker-1)
                                   :types (list :package-name)
                                   :position 0
                                   :parent parent
                                   :start 0
                                   :end position-package-marker-1)
                    (make-fragment :result (subseq token position-package-marker-1 (1+ position-package-marker-2))
                                   :types (list :package-marker)
                                   :position 0
                                   :parent parent
                                   :start position-package-marker-1
                                   :end (1+ position-package-marker-2))
                    (make-fragment :result (subseq token (1+ position-package-marker-2))
                                   :types (list :symbol-name)
                                   :position 1
                                   :parent parent
                                   :start (1+ position-package-marker-2)
                                   :end (length token))))
        parent))
    (position-package-marker-1
      (let ((parent (make-fragment :types (list :external-symbol)
                                   :start 0
                                   :end (length token))))
        (setf (fragment-children parent)
              (list (make-fragment :result (subseq token 0 position-package-marker-1)
                                   :types (list :package-name)
                                   :position 0
                                   :parent parent
                                   :start 0
                                   :end position-package-marker-1)
                    (make-fragment :result (subseq token position-package-marker-1 (1+ position-package-marker-1))
                                   :types (list :package-marker)
                                   :position 0
                                   :parent parent
                                   :start position-package-marker-1
                                   :end (1+ position-package-marker-1))
                    (make-fragment :result (subseq token (1+ position-package-marker-1))
                                   :types (list :symbol-name)
                                   :position 1
                                   :parent parent
                                   :start (1+ position-package-marker-1)
                                   :end (length token))))
        parent))
    (t
      (let ((parent (make-fragment :types (list :local-symbol)
                                   :start 0
                                   :end (length token))))
        (setf (fragment-children parent)
              (list (make-fragment :result token
                                   :types (list :symbol-name)
                                   :position 0
                                   :parent parent
                                   :start 0
                                   :end (length token))))
        parent))))


(defmethod eclector.parse-result:make-expression-result ((client my-client) result children source)
  (let ((parent (make-fragment :result result :start (car source) :end (cdr source) :children children)))
    (prog ((head children)
           (position 0)
           child)
     next
      (when head
        (setq child (pop head))
        (setf (fragment-parent child) parent)
        (setf (fragment-position child) position)
        (incf position)
        (go next)))
    parent))


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
  (make-fragment :types (list reason) :start (car source) :end (cdr source)))


(defun read-fragment (stream)
  ;(ignore-errors
    (handler-bind
        ((error #'eclector.base:recover))
      (eclector.parse-result:read (make-instance 'my-client) stream nil)));)


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
      ;(jupyter:inform :error nil "frag ~A ~A" (fragment-result frag) (fragment-types frag))
      (multiple-value-bind (enclosing-frag contained)
                           (find-fragment frag pos)
        ;(jupyter:inform :error nil "cfrag ~A ~A ~A" contained (fragment-result enclosing-frag) (fragment-types enclosing-frag))
        (when contained
          (return enclosing-frag))))))
