(in-package #:jupyter-widgets)

(defclass checkbox (dom-widget)
  ((description
    :initarg :description
    :accessor widget-description
    :documentation "Button label."
    :trait :unicode)
   (description-tooltip
    :initarg :description-tooltip
    :accessor widget-description-tooltip
    :documentation "Tooltip for the description (defaults to description)."
    :trait :unicode)
   (disabled
    :initarg :disabled
    :initform nil
    :accessor widget-disabled
    :documentation "Enable or disable user changes."
    :trait :boolean)
   (indent
    :initarg :indent
    :initform t
    :accessor widget-indent
    :documentation "Indent the control to align with other controls with a description."
    :trait :boolean)
   (style
    :initarg :style
    :initform (make-widget 'description-style)
    :accessor widget-style
    :documentation "Reference to description style widget."
    :trait :widget)
   (value
    :initarg :value
    :initform nil
    :accessor widget-value
    :documentation "Bool value"
    :trait :boolean))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "CheckboxModel"
    :%model-module +controls-module+
    :%model-module-version +controls-module-version+
    :%view-name "CheckboxView"
    :%view-module +controls-module+
    :%view-module-version +controls-module-version+))

(register-widget checkbox)
