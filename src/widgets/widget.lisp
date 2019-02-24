(in-package #:jupyter-widgets)

(defparameter +protocol-version+ "2.0.0")
(defparameter +base-module+ "@jupyter-widgets/base")
(defparameter +base-module-version+ "1.1.0")
(defparameter +controls-module+ "@jupyter-widgets/controls")
(defparameter +controls-module-version+ "1.4.0")

(defparameter +target-name+ "jupyter.widget")

(defvar *state-lock* nil)

(defclass widget (jupyter:comm jupyter:result)
  ((%model-name
    :initarg :%model-name
    :reader widget-%module-name
    :documentation "Name of the model."
    :sync t)
   (%model-module
    :initarg :%model-module
    :reader widget-%module-module
    :documentation "The namespace for the model."
    :sync t)
   (%model-module-version
    :initarg :%model-module-version
    :reader widget-%module-module-version
    :documentation "A semver requirement for namespace version containing the model."
    :sync t)
   (%view-name
    :initarg :%view-name
    :reader widget-%view-name
    :documentation "Name of the view."
    :sync t)
   (%view-module
    :initarg :%view-module
    :reader widget-%view-module
    :documentation "The namespace for the view."
    :sync t)
   (%view-module-version
    :initarg :%view-module-version
    :reader widget-%view-module-version
    :documentation "A semver requirement for namespace version containing the view."
    :sync t))
  (:metaclass trait-metaclass)
  (:default-initargs :display t
                     :target-name +target-name+))

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
    (when (and (or (not nm) (equal name nm))
               (slot-boundp w name)
               (trait-sync def))
      (jsown:extend-js state
        ((symbol-to-key name)
          (serialize-trait w name nil (slot-value w name)))))
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
      (for state next (jsown:val data "state"))
      (for keywords next (jsown:keywords state))
      (for def in (closer-mop:class-slots (class-of w)))
      (for name next (closer-mop:slot-definition-name def))
      (for key next (symbol-to-key name))
      (when (position key keywords :test #'equal)
        (setf (slot-value w name) (jsown:val state key))))))

(defmethod jupyter:on-comm-message ((w widget) data metadata)
  (declare (ignore metadata))
  (let ((method (jsown:val data "method")))
    (cond
      ((equal "update" method)
        (update-state w data))
      ((equal "request_state" method)
        (send-state w))
      (t (call-next-method)))))

(defmethod on-trait-change ((w widget) name old-value new-value)
  (declare (ignore old-value new-value))
  (send-state w name))

(defmethod jupyter:create-comm ((target-name (eql :|jupyter.widget|)) id data metadata)
  (jupyter:info "create-comm ~A ~A ~A ~A~%" target-name id data metadata))

(defmethod serialize-trait (object type name (value widget))
  (format nil "IPY_MODEL_~A" (jupyter:comm-id value)))

(defun make-widget (class &rest rest &key &allow-other-keys)
  (with-trait-silence
    (let* ((inst (apply 'make-instance class rest))
           (state (to-json-state inst))
           (data (jsown:new-js
                  ("state" state)
                  ("buffer_paths" nil))))
      (jupyter:send-comm-open inst data
        (jsown:new-js ("version" +protocol-version+)))
      inst)))
