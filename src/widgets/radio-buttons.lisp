(in-package #:jupyter-widgets)


(defclass radio-buttons (dom-widget)
  ((%options-labels
    :initarg :%options-labels
    :initform nil
    :accessor widget-%options-labels
    :documentation "The labels for the options."
    :trait :unicode-list)
   (description
    :initarg :description
    :initform ""
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
   (index
    :initarg :index
    :initform nil
    :accessor widget-index
    :documentation "Selected index"
    :trait :int)
   (style
    :initarg :style
    :initform (make-widget 'description-style)
    :accessor widget-style
    :documentation "Reference to description style widget."
    :trait :widget))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "RadioButtonsModel"
    :%model-module +controls-module+
    :%model-module-version +controls-module-version+
    :%view-name "RadioButtonsView"
    :%view-module +controls-module+
    :%view-module-version +controls-module-version+))


(register-widget radio-buttons)
