(in-package #:jupyter-widgets)

(defclass button (dom-widget)
  ((button-style
    :initarg :button-style
    :initform ""
    :accessor widget-button-style
    :documentation "Use a predefined styling for the button."
    :trait :unicode)
   (description
    :initarg :description
    :accessor widget-description
    :documentation "Button label."
    :trait :unicode)
   (disabled
    :initarg :disabled
    :initform nil
    :accessor widget-disabled
    :documentation "Enable or disable user changes."
    :trait :boolean)
   (icon
    :initarg :icon
    :initform ""
    :accessor widget-icon
    :documentation "Font-awesome icon name, without the 'fa-' prefix."
    :trait :unicode)
   (style
    :initarg :style
    :initform (make-widget 'button-style)
    :accessor widget-style
    :documentation "Reference to button style widget."
    :trait :widget)
   (tooltip
    :initarg :tooltip
    :accessor widget-tooltip
    :documentation "Tooltip caption of the button."
    :trait :unicode))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "ButtonModel"
    :%model-module +controls-module+
    :%model-module-version +controls-module-version+
    :%view-name "ButtonView"
    :%view-module +controls-module+
    :%view-module-version +controls-module-version+))

(register-widget button)

(defgeneric on-button-click (w))

(defmethod on-button-click (w))

(defmethod jupyter:on-comm-message ((w button) data metadata)
  (declare (ignore metadata))
  (if (and (equal (jsown:val data "method") "custom")
           (equal (jsown:val (jsown:val data "content") "event") "click"))
    (on-button-click w)
    (call-next-method)))
