(in-package #:jupyter-widgets)

(defclass int-slider (dom-widget)
  ((continuous-update
    :initarg :continuous-update
    :initform t
    :accessor widget-continuous-update
    :documentation "Update the value of the widget as the user is holding the slider."
    :trait :boolean)
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
    :trait :boolean)
   (max
    :initarg :max
    :initform 100
    :accessor widget-max
    :documentation "Max value"
    :trait :integer)
   (min
    :initarg :min
    :initform 0
    :accessor widget-min
    :documentation "Min value"
    :trait :integer)
   (orientation
    :initarg :orientation
    :initform "horizontal"
    :accessor widget-orientation
    :documentation "Vertical or horizontal."
    :trait :unicode)
   (readout
    :initarg :readout
    :initform t
    :accessor widget-readout
    :documentation "Display the current value of the slider next to it."
    :trait :boolean)
   (readout-format
    :initarg :readout-format
    :initform "d"
    :accessor widget-readout-format
    :documentation "Format for the readout"
    :trait :unicode)
   (step
    :initarg :step
    :initform 1
    :accessor widget-step
    :documentation "Minimum step to increment the value"
    :trait :integer)
   (style
    :initarg :style
    :initform (make-widget 'slider-style)
    :accessor widget-style
    :documentation "Reference to slider style widget."
    :trait :widget)
   (value
    :initarg :value
    :initform 0
    :accessor widget-value
    :documentation "Int value"
    :trait :integer))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "IntSliderModel"
    :%model-module +controls-module+
    :%model-module-version +controls-module-version+
    :%view-name "IntSliderView"
    :%view-module +controls-module+
    :%view-module-version +controls-module-version+))
