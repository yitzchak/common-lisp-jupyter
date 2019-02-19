(in-package #:jupyter-widgets)

(defclass button (dom-widget)
  ((description :initarg :description
                :accessor button-description
                :documentation "Button label."
                :sync t)
   (tooltip :initarg :tooltip
            :accessor button-tooltip
            :documentation "Tooltip caption of the button."
            :sync t)
   (disabled :initarg :disabled
             :initform :false
             :accessor button-disabled
             :documentation "Enable or disable user changes."
             :sync t)
   (icon :initarg :icon
         :initform ""
         :accessor button-icon
         :documentation "Font-awesome icon name, without the 'fa-' prefix."
         :sync t))
  (:metaclass trait-metaclass)
  (:default-initargs :%model-name "ButtonModel"
                     :%model-module +model-module+
                     :%model-module-version +model-module-version+
                     :%view-name "ButtonView"
                     :%view-module +view-module+
                     :%view-module-version +view-module-version+))

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
