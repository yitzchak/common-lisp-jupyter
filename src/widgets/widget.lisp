(in-package #:jupyter-widgets)

(defparameter +protocol-version+ "2.0.0")
(defparameter +base-module+ "@jupyter-widgets/base")
(defparameter +base-module-version+ "1.1.0")
(defparameter +controls-module+ "@jupyter-widgets/controls")
(defparameter +controls-module-version+ "1.4.0")
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
    (let* ((initargs (closer-mop:compute-default-initargs class))
           (model-module (eval (second (assoc :%model-module initargs))))
           (model-module-version (eval (second (assoc :%model-module-version initargs))))
           (model-name (eval (second (assoc :%model-name initargs))))
           (view-module (eval (second (assoc :%view-module initargs))))
           (view-module-version (eval (second (assoc :%view-module-version initargs))))
           (view-name (eval (second (assoc :%view-name initargs))))
           (name (widget-registry-name model-module model-module-version
                                       model-name view-module
                                       view-module-version view-name)))
      (when name
        (setf (gethash name *widgets*) (quote ,name))))))

(defclass widget (jupyter:comm jupyter:result)
  ((%model-name
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
    (for type next (trait-type def))
    (when (and (or (not nm) (equal name nm))
               (slot-boundp w name)
               type)
      (jsown:extend-js state
        ((symbol-to-key name)
          (serialize-trait w type name (slot-value w name)))))
    (finally (return state))))

(defun send-state (w &optional name)
  (when (not *state-lock*)
    (let* ((state (to-json-state w name))
           (data (jsown:new-js
                   ("method" "update")
                   ("state" state)
                   ("buffer_paths" nil))))
      (jupyter:send-comm-message w data
        (jsown:new-js ("version" +protocol-version+))))))

(defun update-state (w data)
  (let ((*state-lock* t))
    (iter
      (for state next (jupyter:json-getf data "state"))
      (for keywords next (jsown:keywords state))
      (for def in (closer-mop:class-slots (class-of w)))
      (for name next (closer-mop:slot-definition-name def))
      (for key next (symbol-to-key name))
      (for type next (trait-type def))
      (when (position key keywords :test #'equal)
        (setf (slot-value w name)
          (deserialize-trait w type name (jupyter:json-getf state key)))))))

(defmethod jupyter:on-comm-message ((w widget) data metadata)
  (declare (ignore metadata))
  (switch ((jupyter:json-getf data "method") :test #'equal)
    ("update" (update-state w data))
    ("request_state" (send-state w))
    (otherwise (call-next-method))))

(defmethod on-trait-change :after ((w widget) type name old-value new-value)
  (declare (ignore type old-value new-value))
  (send-state w name))

(defun make-widget (class &rest rest &key &allow-other-keys)
  "Create a Jupyter widget and inform the frontend to create a synchronized model."
  (with-trait-silence
    (let* ((inst (apply 'make-instance class rest))
           (state (to-json-state inst))
           (data (jsown:new-js
                  ("state" state)
                  ("buffer_paths" nil))))
      (jupyter:send-comm-open inst data
        (jsown:new-js ("version" +protocol-version+)))
      inst)))

(defmethod jupyter:create-comm ((target-name (eql :|jupyter.widget|)) id data metadata)
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
    (jupyter:info "~A~%" name)
    (when class
      (make-widget class))))

(defun display (widget)
  "Display a widget in the notebook."
  (jupyter:send-result widget)
  nil)
