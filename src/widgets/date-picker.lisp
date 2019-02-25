(in-package #:jupyter-widgets)

(defclass date-picker (dom-widget)
  ((description
    :initarg :description
    :accessor widget-description
    :documentation "Description of the control."
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
    :trait :bool)
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
    :documentation "The date value."
    :trait :date))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "DatePickerModel"
    :%model-module +controls-module+
    :%model-module-version +controls-module-version+
    :%view-name "DatePickerView"
    :%view-module +controls-module+
    :%view-module-version +controls-module-version+))

(register-widget date-picker)
