(in-package #:jupyter-widgets)

(defclass button (dom-widget)
  ((button-style
    :initarg :button-style
    :initform ""
    :accessor widget-button-style
    :documentation "Use a predefined styling for the button."
    :sync t)
   (description
    :initarg :description
    :accessor widget-description
    :documentation "Button label."
    :sync t)
   (disabled
    :initarg :disabled
    :initform :false
    :accessor widget-disabled
    :documentation "Enable or disable user changes."
    :sync t)
   (icon
    :initarg :icon
    :initform ""
    :accessor widget-icon
    :documentation "Font-awesome icon name, without the 'fa-' prefix."
    :sync t)
   (style
    :initarg :style
    :initform (make-widget 'button-style)
    :accessor widget-style
    :documentation "Reference to button style widget."
    :sync t)
   (tooltip
    :initarg :tooltip
    :accessor widget-tooltip
    :documentation "Tooltip caption of the button."
    :sync t))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "ButtonModel"
    :%model-module +controls-module+
    :%model-module-version +controls-module-version+
    :%view-name "ButtonView"
    :%view-module +controls-module+
    :%view-module-version +controls-module-version+))

(defgeneric on-button-click (w))

(defmethod on-button-click (w))

(defmethod jupyter:on-comm-message ((w button) data metadata)
  (declare (ignore metadata))
  (if (and (equal (jsown:val data "method") "custom")
           (equal (jsown:val (jsown:val data "content") "event") "click"))
    (on-button-click w)
    (call-next-method)))

(defun make-button (description)
  (with-trait-silence
    (let* ((inst (make-instance 'button :description description))
           (state (to-json-state inst))
           (data (jsown:new-js
                  ("state" state)
                  ("buffer_paths" nil))))
      (jupyter:send-comm-open inst data
        (jsown:new-js ("version" +protocol-version+)))
      inst)))
