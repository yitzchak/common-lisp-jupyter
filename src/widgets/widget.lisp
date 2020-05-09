(in-package #:jupyter-widgets)

(defparameter +protocol-version+ "2.0.0")
(defparameter +base-module+ "@jupyter-widgets/base")
(defparameter +base-module-version+ "1.2.0")
(defparameter +controls-module+ "@jupyter-widgets/controls")
(defparameter +controls-module-version+ "1.5.0")
(defparameter +output-module+ "@jupyter-widgets/output")
(defparameter +output-module-version+ "1.0.0")

(defparameter +target-name+ "jupyter.widget")

(defvar *state-lock* nil)
(defvar *widgets* (make-hash-table :test 'equal))

(defun extract-major-version (semver)
  (let* ((v (string-left-trim '(#\0) semver)))
    (subseq v 0 (position #\. v :start (if (equal (char v 0) #\.) 1 0)))))

(defun widget-registry-name (model-module model-module-version model-name
                             view-module view-module-version view-name)
  (when (and model-module model-module-version model-name view-module
             view-module-version view-name)
    (format nil "~A+~A+~A+~A+~A+~A"
      model-module
      (extract-major-version model-module-version)
      model-name
      view-module
      (extract-major-version view-module-version)
      view-name)))

(defmacro register-widget (name)
  `(let ((class (find-class (quote ,name))))
    (closer-mop:finalize-inheritance class)
    (let ((initargs (closer-mop:compute-default-initargs class)))
      (flet ((def-initarg (slot-name)
                ; CMUCL appears to have the default initarg list in a different order.
                (eval (#+cmucl third #-cmucl second (assoc slot-name initargs)))))
        (when-let ((name (widget-registry-name (def-initarg :%model-module)
                                               (def-initarg :%model-module-version)
                                               (def-initarg :%model-name)
                                               (def-initarg :%view-module)
                                               (def-initarg :%view-module-version)
                                               (def-initarg :%view-name))))
          (setf (gethash name *widgets*) (quote ,name)))))))

(defclass widget (jupyter:comm jupyter:result)
  ((on-trait-change
     :initarg :on-trait-change
     :initform nil
     :accessor widget-on-trait-change
     :documentation "Instance specific trait notification")
  (%model-name
     :initarg :%model-name
     :reader widget-%module-name
     :documentation "Name of the model."
     :trait :unicode)
   (%model-module
     :initarg :%model-module
     :reader widget-%module-module
     :documentation "The namespace for the model."
     :trait :unicode)
   (%model-module-version
     :initarg :%model-module-version
     :reader widget-%module-module-version
     :documentation "A semver requirement for namespace version containing the model."
     :trait :unicode)
   (%view-name
     :initarg :%view-name
     :reader widget-%view-name
     :documentation "Name of the view."
     :trait :unicode)
   (%view-module
     :initarg :%view-module
     :reader widget-%view-module
     :documentation "The namespace for the view."
     :trait :unicode)
   (%view-module-version
     :initarg :%view-module-version
     :reader widget-%view-module-version
     :documentation "A semver requirement for namespace version containing the view."
     :trait :unicode))
  (:metaclass trait-metaclass)
  (:default-initargs
    :display-data t
    :target-name +target-name+)
  (:documentation "Base class for all Jupyter widgets."))

(defmethod jupyter:render ((w widget))
  (jsown:new-js
    ("text/plain" "A Jupyter Widget")
    ("application/vnd.jupyter.widget-view+json"
      (jsown:new-js
        ("version_major" 2)
        ("version_minor" 0)
        ("model_id" (jupyter:comm-id w))))))

(defun symbol-to-key (s)
  (substitute #\_ #\%
    (substitute #\_ #\-
      (string-downcase (symbol-name s)))))

(defmethod to-json-state (w &optional nm)
  (iter
    (with state = (jsown:new-js))
    (for def in (closer-mop:class-slots (class-of w)))
    (for name next (closer-mop:slot-definition-name def))
    (for trait-name next (trait-name name))
    (for type next (trait-type def))
    (when (and (or (not nm) (eql trait-name nm))
               (slot-boundp w name)
               type)
      (jsown:extend-js state
        ((symbol-to-key name)
          (serialize-trait w type trait-name (slot-value w name)))))
    (finally (return state))))

(defun extract-buffers (state &optional path)
  (cond
    ((and (listp state) (eq (first state) :obj))
      (iter
        (for (k . v) in (cdr state))
        (cond
          ((and (vectorp v) (equal (array-element-type v) '(unsigned-byte 8)))
            (collect (append path (list k)) into buffer-paths)
            (collect v into buffers)
            (jsown:remkey state k))
          (t
            (multiple-value-bind (sub-buffer-paths sub-buffers) (extract-buffers v (append path (list k)))
              (appending sub-buffer-paths into buffer-paths)
              (appending sub-buffers into buffers))))
        (finally
          (return (values buffer-paths buffers)))))
    ((listp state)
      (iter
        (for v in-sequence state with-index i)
        (cond
          ((and (vectorp v) (equal (array-element-type v) '(unsigned-byte 8)))
            (collect (append path (list i)) into buffer-paths)
            (collect v into buffers)
            (setf (elt state i) :null))
          (t
            (multiple-value-bind (sub-buffer-paths sub-buffers) (extract-buffers v (append path (list i)))
              (appending sub-buffer-paths into buffer-paths)
              (appending sub-buffers into buffers))))
        (finally
          (return (values buffer-paths buffers)))))
    (t
      (values nil nil))))

(defun inject-buffer (state buffer-path buffer)
  (let ((node (car buffer-path))
        (rest (cdr buffer-path)))
    (if rest
      (inject-buffer (if (stringp node)
                       (jsown:val state node)
                       (elt state node))
                     rest buffer)
      (if (stringp node)
        (setf (jsown:val state node) buffer)
        (setf (elt state node) buffer)))))

(defun inject-buffers (state buffer-paths buffers)
  (iter
    (for buffer-path in buffer-paths)
    (for buffer in buffers)
    (inject-buffer state buffer-path buffer)))

(defun send-state (w &optional name)
  (when (not *state-lock*)
    (let ((state (to-json-state w name)))
      (multiple-value-bind (buffer-paths buffers) (extract-buffers state)
        (jupyter:send-comm-message w
          (jsown:new-js ("method" "update")
                        ("state" state)
                        ("buffer_paths" buffer-paths))
          (jsown:new-js ("version" +protocol-version+))
          buffers)))))

(defun update-state (w data buffers)
  (let ((*state-lock* t))
    (iter
      (with state = (jupyter:json-getf data "state"))
      (with buffer-paths = (jupyter:json-getf data "buffer_paths"))
      (inject-buffers state buffer-paths buffers)
      (with keywords = (jsown:keywords state))
      (for def in (closer-mop:class-slots (class-of w)))
      (for name next (closer-mop:slot-definition-name def))
      (for trait-name next (trait-name name))
      (for key next (symbol-to-key name))
      (for type next (trait-type def))
      (when (position key keywords :test #'equal)
        (setf (slot-value w name)
          (deserialize-trait w type trait-name (jupyter:json-getf state key)))))))

(defun send-custom (widget content &optional buffers)
  (jupyter:send-comm-message widget
    (jsown:new-js ("method" "custom")
                  ("content" content))
    (jsown:new-js ("version" +protocol-version+))
    buffers))

(defgeneric on-custom-message (widget content buffers))

(defmethod on-custom-message (widget content buffers))

(defmethod jupyter:on-comm-message ((w widget) data metadata buffers)
  (declare (ignore metadata))
  (switch ((jupyter:json-getf data "method") :test #'equal)
    ("update"
      (update-state w data buffers))
    ("request_state"
      (send-state w))
    ("custom"
      (on-custom-message w (jupyter:json-getf data "content") buffers))
    (otherwise
      (call-next-method))))

(defmethod on-trait-change :after ((w widget) type name old-value new-value)
  (when-let ((handler (cdr (assoc name (widget-on-trait-change w)))))
    (funcall handler w type name old-value new-value))
  (send-state w name))

(defmethod initialize-instance :around ((instance widget) &rest rest &key &allow-other-keys)
  (declare (ignore rest))
  (with-trait-silence
    (call-next-method)
    (let ((state (to-json-state instance)))
      (multiple-value-bind (buffer-paths buffers) (extract-buffers state)
        (jupyter:send-comm-open instance
          (jsown:new-js ("state" state)
                        ("buffer_paths" buffer-paths))
          (jsown:new-js ("version" +protocol-version+))
          buffers)))))

(defmethod jupyter:create-comm ((target-name (eql :|jupyter.widget|)) id data metadata buffers)
  (let* ((state (jupyter:json-getf data "state"))
         (model-name (jupyter:json-getf state "_model_name"))
         (model-module (jupyter:json-getf state "_model_module"))
         (model-module-version (jupyter:json-getf state "_model_module_version"))
         (view-name (jupyter:json-getf state "_view_name"))
         (view-module (jupyter:json-getf state "_view_module"))
         (view-module-version (jupyter:json-getf state "_view_module_version"))
         (name (widget-registry-name model-module model-module-version
                                     model-name view-module
                                     view-module-version view-name))
         (class (gethash name *widgets*)))
    (when class
      (with-trait-silence
        (let ((w (make-instance class)))
          (update-state w data buffers)
          w)))))

(defun observe (instance name handler)
  (with-slots (on-trait-change) instance
    (let ((pair (assoc name on-trait-change)))
      (if pair
        (rplacd pair handler)
        (push (cons name handler) on-trait-change)))))

(defun display (widget)
  "Display a widget in the notebook."
  (jupyter:send-result widget)
  nil)
